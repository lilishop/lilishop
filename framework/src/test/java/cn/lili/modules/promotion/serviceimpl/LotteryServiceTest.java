package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.exception.ServiceException;
import cn.lili.common.enums.ResultCode;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.mapper.*;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.promotion.entity.dos.*;
import cn.lili.modules.promotion.mapper.*;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.modules.promotion.support.CouponReceiveSupport;
import org.junit.jupiter.api.*;
import java.time.*;
import java.util.*;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * 免费抽奖业务边界单元测试：幂等、限次、固定概率、发奖失败和发布冻结。
 * 使用模拟存储，不代表真实数据库并发锁、事务回滚或日界线集成验收。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
class LotteryServiceTest {
    private final LotteryActivityMapper activities = mock(LotteryActivityMapper.class);
    private final LotteryPrizeMapper prizes = mock(LotteryPrizeMapper.class);
    private final LotteryRecordMapper records = mock(LotteryRecordMapper.class);
    private final MemberMapper members = mock(MemberMapper.class);
    private final MemberService memberService = mock(MemberService.class);
    private final MemberPointsHistoryMapper histories = mock(MemberPointsHistoryMapper.class);
    private final CouponMapper coupons = mock(CouponMapper.class);
    private final MemberCouponService memberCoupons = mock(MemberCouponService.class);
    private final CouponReceiveSupport receive = mock(CouponReceiveSupport.class);
    private final Clock clock = mock(Clock.class);
    private final LotteryServiceImpl service = new LotteryServiceImpl(activities, prizes, records, members, memberService, histories, coupons, memberCoupons, receive, clock);
    private Member member;
    private LotteryActivity activity;
    private LotteryPrize reward;
    private LotteryPrize thanks;

    /**
     * 准备每天一次、全概率积分奖池；THANKS零概率仅用于稳定抽奖分支，正式配置会拒绝此概率。
     */
    @BeforeEach void setup() {
        when(clock.instant()).thenReturn(Instant.parse("2026-11-14T16:00:00Z"));
        member = new Member(); member.setId("member"); member.setUsername("会员"); member.setDisabled(true); member.setPoint(10L); member.setTotalPoint(10L);
        activity = new LotteryActivity(); activity.setId("activity"); activity.setName("每日抽奖"); activity.setDescription("免费抽奖"); activity.setDailyLimit(1);
        activity.setEnabled(true); activity.setPublished(true); activity.setStartTime(new Date(0)); activity.setEndTime(Date.from(Instant.parse("2030-01-01T00:00:00Z")));
        reward = new LotteryPrize(); reward.setId("reward"); reward.setName("积分奖"); reward.setType("POINTS"); reward.setWeight(10000); reward.setTotal(1); reward.setIssued(0); reward.setPoints(10L);
        thanks = new LotteryPrize(); thanks.setId("thanks"); thanks.setName("谢谢参与"); thanks.setType("THANKS"); thanks.setWeight(0); thanks.setTotal(0); thanks.setIssued(0); thanks.setPoints(0L);
        when(members.lockPointMember("member")).thenReturn(member);
        when(activities.lock("activity")).thenReturn(activity);
        when(prizes.lockPool("activity")).thenReturn(List.of(reward, thanks));
        when(records.selectCount(any())).thenReturn(0L);
        when(records.insert(any(LotteryRecord.class))).thenReturn(1);
        when(prizes.updateById(any(LotteryPrize.class))).thenReturn(1);
        when(activities.updateById(any(LotteryActivity.class))).thenReturn(1);
        when(memberService.updateMemberPoint(anyLong(), anyString(), anyString(), anyString(), anyString())).thenReturn(true);
        when(histories.selectCount(any())).thenReturn(0L, 1L);
    }

    /**
     * 重试不受下架及跨天影响，不再次发奖或消耗次数。
     */
    @Test void retriesReturnOriginalBeforeReadingActivity() {
        LotteryRecord prior = new LotteryRecord(); prior.setActivityId("activity"); prior.setDrawDay(LocalDate.of(2020, 1, 1));
        when(records.findRequest("member", "req")).thenReturn(prior);
        assertThat(service.draw("activity", "member", "req")).isSameAs(prior);
        verifyNoInteractions(activities, prizes, memberService, histories, receive);
        verify(records, never()).insert(any(LotteryRecord.class));
    }

    /**
     * 请求号不得跨活动复用。
     */
    @Test void rejectsRequestFromAnotherActivity() {
        LotteryRecord prior = new LotteryRecord(); prior.setActivityId("other"); when(records.findRequest("member", "req")).thenReturn(prior);
        assertThatThrownBy(() -> service.draw("activity", "member", "req")).hasMessageContaining("其他活动");
        verifyNoInteractions(memberService, receive);
    }

    /**
     * 非法请求在数据库操作前拒绝。
     */
    @Test void rejectsInvalidRequest() {
        assertThatThrownBy(() -> service.draw("activity", "member", "bad/request")).isInstanceOf(ServiceException.class);
        verifyNoInteractions(members, records);
    }

    /**
     * 禁用账号不能参与。
     */
    @Test void rejectsDisabledMember() {
        member.setDisabled(false);
        assertThatThrownBy(() -> service.draw("activity", "member", "req")).hasMessageContaining("禁用");
        verifyNoInteractions(prizes, memberService);
    }

    /**
     * 每日限次达到时不会进行判奖和发放。
     */
    @Test void rejectsExhaustedDailyChances() {
        when(records.selectCount(any())).thenReturn(1L);
        assertThatThrownBy(() -> service.draw("activity", "member", "req")).hasMessageContaining("次数已用完");
        verifyNoInteractions(prizes, memberService, receive);
    }

    /**
     * 有效区间不包含已结束时间。
     */
    @Test void rejectsEndedActivity() {
        activity.setEndTime(new Date(0));
        assertThatThrownBy(() -> service.draw("activity", "member", "req")).hasMessageContaining("已结束");
        verifyNoInteractions(prizes, memberService);
    }

    /**
     * 成功积分奖增加库存计数并留下北京时间日期和积分快照。
     */
    @Test void awardsPointsAndRecordsEvidence() {
        LotteryRecord result = service.draw("activity", "member", "req");
        assertThat(result.getPrizeType()).isEqualTo("POINTS"); assertThat(result.getPoints()).isEqualTo(10L);
        assertThat(result.getDrawDay()).isEqualTo(LocalDate.of(2026, 11, 15));
        assertThat(reward.getIssued()).isEqualTo(1);
        verify(memberService).updateMemberPoint(10L, "INCREASE", "member", "抽奖奖励：参与「每日抽奖」，获得「积分奖」，奖励10积分", "LOTTERY");
        verify(records).insert(result);
    }

    /**
     * 最后一份已发完时落未中奖，不转移概率、不发放积分。
     */
    @Test void exhaustedPrizeBecomesThanksWithoutReroll() {
        reward.setIssued(1);
        LotteryRecord result = service.draw("activity", "member", "req");
        assertThat(result.getPrizeId()).isEqualTo("thanks"); assertThat(result.getPoints()).isZero();
        verifyNoInteractions(memberService, histories, receive);
        verify(prizes, never()).updateById(any(LotteryPrize.class)); verify(records).insert(result);
    }

    /**
     * 流水被切面吞掉时抛异常，交由事务拦截器回滚，不写成功抽奖结果。
     */
    @Test void rejectsMissingPointHistory() {
        when(histories.selectCount(any())).thenReturn(0L);
        assertThatThrownBy(() -> service.draw("activity", "member", "req")).hasMessageContaining("积分流水");
        verify(records, never()).insert(any(LotteryRecord.class)); verify(prizes, never()).updateById(any(LotteryPrize.class));
    }

    /**
     * 多次中奖可使用相同业务文案，仍核对本次新增流水。
     */
    @Test void awardsPointsWhenOtherRewardHistoriesExist() {
        when(histories.selectCount(any())).thenReturn(3L, 4L);
        assertThat(service.draw("activity", "member", "req").getPrizeType()).isEqualTo("POINTS");
        verify(memberService).updateMemberPoint(10L, "INCREASE", "member", "抽奖奖励：参与「每日抽奖」，获得「积分奖」，奖励10积分", "LOTTERY");
    }

    /**
     * 已有奖励流水不能掩盖本次积分流水保存失败。
     */
    @Test void rejectsMissingPointHistoryEvenWhenOtherRewardsExist() {
        when(histories.selectCount(any())).thenReturn(3L, 3L);
        assertThatThrownBy(() -> service.draw("activity", "member", "req")).hasMessageContaining("积分流水");
        verify(records, never()).insert(any(LotteryRecord.class));
        verify(prizes, never()).updateById(any(LotteryPrize.class));
    }

    /**
     * 积分服务失败不能写成功记录。
     */
    @Test void rejectsPointAwardFailure() {
        when(memberService.updateMemberPoint(anyLong(), anyString(), anyString(), anyString(), anyString())).thenReturn(false);
        assertThatThrownBy(() -> service.draw("activity", "member", "req")).hasMessageContaining("积分发放失败");
        verify(records, never()).insert(any(LotteryRecord.class));
    }

    /**
     * 固定券过期时落未中奖，避免记录中奖而券实际不可用。
     */
    @Test void expiredCouponBecomesThanks() {
        reward.setType("COUPON"); reward.setCouponId("coupon"); reward.setPoints(0L);
        LotteryRecord result = service.draw("activity", "member", "req");
        assertThat(result.getPrizeType()).isEqualTo("THANKS"); verifyNoInteractions(memberCoupons, receive, memberService);
    }

    /**
     * 正常券奖保留会员券关联；发券失败不能写结果。
     */
    @Test void couponSaveFailureAbortsResult() {
        reward.setType("COUPON"); reward.setCouponId("coupon"); reward.setPoints(0L);
        Coupon coupon = new Coupon(); coupon.setId("coupon"); coupon.setStoreId("0"); coupon.setGetType("ACTIVITY");
        coupon.setRangeDayType("DYNAMICTIME"); coupon.setEffectiveDays(7); coupon.setPublishNum(10); coupon.setReceivedNum(0);
        when(coupons.lockCoupon("coupon")).thenReturn(coupon);
        when(memberCoupons.save(any(MemberCoupon.class))).thenReturn(false);
        assertThatThrownBy(() -> service.draw("activity", "member", "req")).hasMessageContaining("优惠券发放失败");
        verify(receive).receiveCoupon("coupon", 1); verify(records, never()).insert(any(LotteryRecord.class));
    }

    /**
     * 已发布活动即使下架也不可改规则。
     */
    @Test void publishedActivityCannotBeEdited() {
        activity.setEnabled(false);
        assertThatThrownBy(() -> service.configure(activity)).hasMessageContaining("不可修改规则");
        verifyNoInteractions(prizes);
    }

    /**
     * 万分概率的分界点准确，零概率奖项不会被选中。
     */
    @Test void fixedProbabilityBoundaries() {
        reward.setWeight(1234); thanks.setWeight(8766);
        List<LotteryPrize> pool = List.of(reward, thanks);
        assertThat(LotteryServiceImpl.select(pool, 0)).isSameAs(reward);
        assertThat(LotteryServiceImpl.select(pool, 1233)).isSameAs(reward);
        assertThat(LotteryServiceImpl.select(pool, 1234)).isSameAs(thanks);
        assertThat(LotteryServiceImpl.select(pool, 9999)).isSameAs(thanks);
    }

    /**
     * 北京时间零点前后分别记入两个自然日，不依赖服务器默认时区。
     */
    @Test void usesBeijingMidnightForDailyCounter() {
        when(clock.instant()).thenReturn(Instant.parse("2026-11-14T15:59:59Z"));
        assertThat(service.draw("activity", "member", "before").getDrawDay()).isEqualTo(LocalDate.of(2026, 11, 14));
        when(clock.instant()).thenReturn(Instant.parse("2026-11-14T16:00:00Z"));
        assertThat(service.draw("activity", "member", "after").getDrawDay()).isEqualTo(LocalDate.of(2026, 11, 15));
    }

    /**
     * 开始时刻允许参与，结束时刻精确拒绝。
     */
    @Test void activityIntervalIsStartInclusiveEndExclusive() {
        activity.setStartTime(Date.from(clock.instant()));
        assertThat(service.draw("activity", "member", "start").getPrizeType()).isEqualTo("POINTS");
        activity.setEndTime(Date.from(clock.instant()));
        assertThatThrownBy(() -> service.draw("activity", "member", "end")).hasMessageContaining("已结束");
    }

    /**
     * 不合法概率不能发布为草稿，后端不依赖前端表单校验。
     */
    @Test void rejectsInvalidProbabilityAndMissingThanks() {
        activity.setPublished(false); activity.setPrizes(List.of(reward, thanks));
        reward.setWeight(9000); thanks.setWeight(100);
        assertThatThrownBy(() -> service.configure(activity)).hasMessageContaining("合计10000");
        thanks.setType("POINTS"); thanks.setTotal(1); thanks.setPoints(10L); thanks.setWeight(1000);
        assertThatThrownBy(() -> service.configure(activity)).hasMessageContaining("谢谢参与");
    }

    /**
     * 绕过前端提交零、负数或小数百分比时不能保存草稿。
     */
    @Test void rejectsZeroNegativeAndFractionalPercentages() {
        activity.setPublished(false); activity.setPrizes(List.of(reward, thanks));
        for (int weight : new int[] { 0, -100, 1, 50, 101, 10001 }) {
            reward.setWeight(weight); thanks.setWeight(10000 - weight);
            assertThatThrownBy(() -> service.configure(activity)).hasMessageContaining("正整数");
        }
        verify(prizes, never()).insert(any(LotteryPrize.class));
    }

    /**
     * 百分比下界1%和99%可共同保存，原库存和整数万分点口径不变。
     */
    @Test void acceptsWholePercentagesWithoutChangingStorageUnit() {
        activity.setPublished(false); activity.setPrizes(List.of(reward, thanks));
        reward.setWeight(100); thanks.setWeight(9900);
        when(prizes.insert(any(LotteryPrize.class))).thenReturn(1);
        LotteryActivity saved = service.configure(activity);
        assertThat(saved.getPrizes()).extracting(LotteryPrize::getWeight).containsExactly(100, 9900);
        assertThat(saved.getPrizes().getFirst().getTotal()).isEqualTo(1);
    }

    /**
     * 旧草稿首次发布也需满足正整数规则，已经发布的历史活动不被改写。
     */
    @Test void firstPublicationRejectsLegacyFractionalDraftButPreservesPublishedPool() {
        activity.setPublished(false); reward.setWeight(1234); thanks.setWeight(8766);
        assertThatThrownBy(() -> service.status("activity", true)).hasMessageContaining("正整数");
        activity.setPublished(true);
        service.status("activity", true);
        assertThat(reward.getWeight()).isEqualTo(1234);
        assertThat(thanks.getWeight()).isEqualTo(8766);
    }

    /**
     * 券奖成功写入会员券ID；仅已知领取资格错误可降为未中奖，系统异常终止事务。
     */
    @Test void couponAwardPreservesReferenceAndDoesNotHideSystemErrors() {
        reward.setType("COUPON"); reward.setCouponId("coupon"); reward.setPoints(0L);
        Coupon coupon = new Coupon(); coupon.setId("coupon"); coupon.setStoreId("0"); coupon.setGetType("ACTIVITY");
        coupon.setRangeDayType("DYNAMICTIME"); coupon.setEffectiveDays(7); coupon.setPublishNum(10); coupon.setReceivedNum(0);
        when(coupons.lockCoupon("coupon")).thenReturn(coupon);
        when(memberCoupons.save(any(MemberCoupon.class))).thenAnswer(call -> { call.<MemberCoupon>getArgument(0).setId("issued"); return true; });
        assertThat(service.draw("activity", "member", "success").getMemberCouponId()).isEqualTo("issued");
        reward.setIssued(0);
        doThrow(new ServiceException(ResultCode.COUPON_LIMIT_ERROR)).when(memberCoupons).checkCouponLimit("coupon", "member");
        assertThat(service.draw("activity", "member", "limit").getPrizeType()).isEqualTo("THANKS");
        doThrow(new ServiceException(ResultCode.ERROR)).when(memberCoupons).checkCouponLimit("coupon", "member");
        assertThatThrownBy(() -> service.draw("activity", "member", "system")).isInstanceOf(ServiceException.class);
    }
}
