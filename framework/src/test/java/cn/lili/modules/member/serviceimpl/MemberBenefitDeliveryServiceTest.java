package cn.lili.modules.member.serviceimpl;

import cn.lili.modules.member.service.MemberBenefitDeliveryService;

import cn.lili.common.exception.ServiceException;
import cn.lili.modules.member.entity.dos.*;
import cn.lili.modules.member.mapper.MemberBenefitDeliveryMapper;
import cn.lili.modules.member.mapper.PaidMemberOrderMapper;
import cn.lili.modules.member.mapper.MemberBenefitMapper;
import cn.lili.modules.member.service.MemberService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.SimpleTransactionStatus;
import java.util.function.BiConsumer;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * 权益账本快照、防重、失败事务调用及付费来源有效期测试；使用模拟存储，不验证真实数据库事务。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
class MemberBenefitDeliveryServiceTest {
    /** 模拟发放账本预占、行锁及结果写入。 */
    private final MemberBenefitDeliveryMapper mapper=mock(MemberBenefitDeliveryMapper.class);
    /** 模拟领取客户查询及行锁。 */
    private final MemberService members=mock(MemberService.class);
    /** 模拟事务API，仅验证提交和回滚调用，不代表真实数据库事务。 */
    private final PlatformTransactionManager transactions=mock(PlatformTransactionManager.class);
    /** 被测试的权益逐项发放服务。 */
    private final MemberBenefitDeliveryService service=new MemberBenefitDeliveryServiceImpl();
    /** 用于补发、防重和失败场景的账本快照。 */
    private final MemberBenefitDelivery record=new MemberBenefitDelivery();
    /** 模拟付费来源订单的状态、归属及有效期。 */
    private final PaidMemberOrderMapper paidOrders=mock(PaidMemberOrderMapper.class);
    /** 模拟权益当前开启状态。 */
    private final MemberBenefitMapper benefits=mock(MemberBenefitMapper.class);
    /**
     * 准备失败账本、客户及模拟事务管理器，用于验证发放回调与事务API调用。
     * 不连接真实数据库，不能据此认定多连接锁及数据库回滚已验收。
     */
    @BeforeEach void setup() {
        ReflectionTestUtils.setField(service,"baseMapper",mapper); ReflectionTestUtils.setField(service,"memberService",members);
        ReflectionTestUtils.setField(service,"transactionManager",transactions);
        ReflectionTestUtils.setField(service,"paidOrders",paidOrders);
        ReflectionTestUtils.setField(service,"benefits",benefits);
        when(transactions.getTransaction(any())).thenAnswer(call->new SimpleTransactionStatus());
        record.setId("delivery");record.setMemberId("member");record.setBenefitId("benefit");record.setBenefitType("GIFT_POINT");
        record.setBenefitConfig("{\"giftPoint\":100}");record.setStatus("FAILED");record.setAttempts(1);
        when(mapper.lock("delivery")).thenReturn(record);
        Member member=new Member();member.setId("member");when(members.getOne(any())).thenReturn(member);
    }
    /**
     * 验证补发沿用账本中的原始配置快照，成功后再次请求不重复执行发放回调。
     */
    @Test void retryUsesOriginalConfigAndSuccessfulRecordCannotIssueAgain() {
        @SuppressWarnings("unchecked") BiConsumer<Member,MemberBenefit> grant=mock(BiConsumer.class);
        service.retry(record,grant);service.retry(record,grant);
        var benefit=org.mockito.ArgumentCaptor.forClass(MemberBenefit.class);
        verify(grant,times(1)).accept(any(),benefit.capture());
        assertThat(benefit.getValue().getBenefitConfig()).isEqualTo("{\"giftPoint\":100}");
        assertThat(record.getStatus()).isEqualTo("SUCCESS");
    }
    /**
     * 模拟发券库存不足，验证发放事务调用回滚、另行保存失败原因且不写成功状态。
     */
    @Test void failedGrantRollsBackAndPersistsReasonSeparately() {
        assertThatThrownBy(()->service.retry(record,(member,benefit)->{throw new IllegalStateException("优惠券库存不足");})).isInstanceOf(ServiceException.class);
        verify(transactions).rollback(any());
        verify(mapper).failed("delivery","优惠券库存不足");
        verify(mapper,never()).updateById(any(MemberBenefitDelivery.class));
    }
    /**
     * 验证SUCCESS记录直接跳过，不执行回调，也不因重复请求改写为失败。
     */
    @Test void successNeverBecomesFailureOnRepeatedRetry() {
        record.setStatus("SUCCESS");
        service.retry(record,(member,benefit)->{throw new AssertionError("不应重复发放");});
        verify(mapper,never()).failed(anyString(),anyString());
    }
    /**
     * 构造已支付且关联目标权益的来源订单，并配置当前权益开启状态。
     *
     * @param start 有效期开始的Unix时间戳，单位毫秒
     * @param end 有效期结束的Unix时间戳，单位毫秒
     * @return 模拟查询返回的付费会员订单
     */
    private PaidMemberOrder paidOrder(long start, long end) {
        record.setSourceType("PAID_MEMBER"); record.setSourceRef("paid-order");
        PaidMemberOrder order=new PaidMemberOrder();order.setMemberId("member");order.setStatus("PAID");
        order.setStartTime(new java.util.Date(start));order.setEndTime(new java.util.Date(end));
        order.setConfigSnapshot("{\"benefitIds\":[\"benefit\"]}");
        when(paidOrders.selectById("paid-order")).thenReturn(order);
        MemberBenefit benefit=new MemberBenefit();benefit.setBenefitState("OPEN");
        when(benefits.selectById("benefit")).thenReturn(benefit);
        return order;
    }
    /**
     * 验证未来续费区间及已到期订单均不能发放权益，失败时不调用实际发放。
     */
    @Test void futureRenewalAndExpiredOrderCannotDeliver() {
        long now=System.currentTimeMillis();
        var order=paidOrder(now+60000,now+120000);
        BiConsumer<Member,MemberBenefit> grant=mock(BiConsumer.class);
        assertThatThrownBy(()->service.retry(record,grant)).isInstanceOf(ServiceException.class);
        order.setStartTime(new java.util.Date(now-120000));order.setEndTime(new java.util.Date(now-60000));
        assertThatThrownBy(()->service.retry(record,grant)).isInstanceOf(ServiceException.class);
        verifyNoInteractions(grant);
    }
    /**
     * 验证权益停用时拒绝发放，重新开启后可补发，成功记录再次请求不重复兑现。
     */
    @Test void activePaidGrantIsOnceAndClosedBenefitCannotDeliver() {
        long now=System.currentTimeMillis();paidOrder(now-60000,now+60000);
        BiConsumer<Member,MemberBenefit> grant=mock(BiConsumer.class);
        MemberBenefit benefit=new MemberBenefit();benefit.setBenefitState("CLOSE");
        when(benefits.selectById("benefit")).thenReturn(benefit);
        assertThatThrownBy(()->service.retry(record,grant)).isInstanceOf(ServiceException.class);
        verifyNoInteractions(grant);
        benefit.setBenefitState("OPEN");service.retry(record,grant);service.retry(record,grant);
        verify(grant,times(1)).accept(any(),any());
    }
}
