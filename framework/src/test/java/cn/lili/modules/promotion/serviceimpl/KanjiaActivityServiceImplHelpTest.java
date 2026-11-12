package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.promotion.entity.dos.KanjiaActivity;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityGoods;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityLog;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityDTO;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.mapper.KanJiaActivityMapper;
import cn.lili.modules.promotion.service.KanjiaActivityGoodsService;
import cn.lili.modules.promotion.service.KanjiaActivityLogService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.concurrent.TimeUnit;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 砍价活动帮砍防重及并发防护单元测试。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
class KanjiaActivityServiceImplHelpTest {

    private KanjiaActivityServiceImpl kanjiaActivityService;
    private KanjiaActivityGoodsService kanjiaActivityGoodsService;
    private KanjiaActivityLogService kanjiaActivityLogService;
    private MemberService memberService;
    private RedissonClient redissonClient;
    private RLock rLock;
    private KanJiaActivityMapper kanJiaActivityMapper;

    @BeforeEach
    void setUp() throws InterruptedException {
        kanjiaActivityService = spy(new KanjiaActivityServiceImpl());
        kanjiaActivityGoodsService = mock(KanjiaActivityGoodsService.class);
        kanjiaActivityLogService = mock(KanjiaActivityLogService.class);
        memberService = mock(MemberService.class);
        redissonClient = mock(RedissonClient.class);
        rLock = mock(RLock.class);
        kanJiaActivityMapper = mock(KanJiaActivityMapper.class);

        ReflectionTestUtils.setField(kanjiaActivityService, "kanjiaActivityGoodsService", kanjiaActivityGoodsService);
        ReflectionTestUtils.setField(kanjiaActivityService, "kanjiaActivityLogService", kanjiaActivityLogService);
        ReflectionTestUtils.setField(kanjiaActivityService, "memberService", memberService);
        ReflectionTestUtils.setField(kanjiaActivityService, "redissonClient", redissonClient);
        ReflectionTestUtils.setField(kanjiaActivityService, "baseMapper", kanJiaActivityMapper);

        when(redissonClient.getLock(anyString())).thenReturn(rLock);
        when(rLock.tryLock(anyLong(), anyLong(), any(TimeUnit.class))).thenReturn(true);
        when(rLock.isHeldByCurrentThread()).thenReturn(true);
    }

    private AuthUser mockUser() {
        return new AuthUser("testUser", "10001", "testNick", "http://avatar.jpg", UserEnums.MEMBER);
    }

    private Member mockMember() {
        Member member = new Member();
        member.setId("10001");
        member.setUsername("testUser");
        member.setFace("http://avatar.jpg");
        return member;
    }

    private KanjiaActivity mockActivity() {
        KanjiaActivity activity = new KanjiaActivity();
        activity.setId("act_1");
        activity.setKanjiaActivityGoodsId("goods_1");
        activity.setStatus(PromotionsStatusEnum.START.name());
        activity.setSurplusPrice(100.0);
        return activity;
    }

    private KanjiaActivityGoods mockGoods() {
        KanjiaActivityGoods goods = new KanjiaActivityGoods();
        goods.setId("goods_1");
        goods.setLowestPrice(10.0);
        goods.setHighestPrice(10.0);
        return goods;
    }

    @Test
    @DisplayName("正常帮砍成功：记录帮砍日志并更新活动剩余金额")
    void helpKanJia_success() {
        try (MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class)) {
            userContextMock.when(UserContext::getCurrentUser).thenReturn(mockUser());

            doReturn(mockActivity()).when(kanjiaActivityService).getById("act_1");
            doReturn(true).when(kanjiaActivityService).updateById(any(KanjiaActivity.class));
            when(memberService.getById("10001")).thenReturn(mockMember());
            when(kanjiaActivityGoodsService.getById("goods_1")).thenReturn(mockGoods());
            when(kanjiaActivityLogService.count(any(LambdaQueryWrapper.class))).thenReturn(0L);

            KanjiaActivityLog createdLog = new KanjiaActivityLog();
            createdLog.setId("1");
            createdLog.setSurplusPrice(90.0);
            when(kanjiaActivityLogService.addKanJiaActivityLog(any(KanjiaActivityDTO.class))).thenReturn(createdLog);

            KanjiaActivityLog result = kanjiaActivityService.helpKanJia("act_1");

            assertThat(result).isNotNull();
            assertThat(result.getSurplusPrice()).isEqualTo(90.0);
            verify(rLock).unlock();
        }
    }

    @Test
    @DisplayName("并发重复点击：获取锁失败时抛出 PROMOTION_LOG_EXIST 异常")
    void helpKanJia_lockFailed_throwsException() throws InterruptedException {
        try (MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class)) {
            userContextMock.when(UserContext::getCurrentUser).thenReturn(mockUser());
            when(rLock.tryLock(anyLong(), anyLong(), any(TimeUnit.class))).thenReturn(false);

            assertThatThrownBy(() -> kanjiaActivityService.helpKanJia("act_1"))
                    .isInstanceOf(ServiceException.class)
                    .extracting("resultCode")
                    .isEqualTo(ResultCode.PROMOTION_LOG_EXIST);
        }
    }

    @Test
    @DisplayName("已参与帮砍：再次帮砍时检查 count > 0 抛出 PROMOTION_LOG_EXIST 异常")
    void helpKanJia_alreadyHelped_throwsException() {
        try (MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class)) {
            userContextMock.when(UserContext::getCurrentUser).thenReturn(mockUser());

            doReturn(mockActivity()).when(kanjiaActivityService).getById("act_1");
            when(memberService.getById("10001")).thenReturn(mockMember());
            when(kanjiaActivityGoodsService.getById("goods_1")).thenReturn(mockGoods());
            when(kanjiaActivityLogService.count(any(LambdaQueryWrapper.class))).thenReturn(1L);

            assertThatThrownBy(() -> kanjiaActivityService.helpKanJia("act_1"))
                    .isInstanceOf(ServiceException.class)
                    .extracting("resultCode")
                    .isEqualTo(ResultCode.PROMOTION_LOG_EXIST);

            verify(rLock).unlock();
        }
    }

    @Test
    @DisplayName("数据库唯一索引兜底：触发 DuplicateKeyException 转换为 PROMOTION_LOG_EXIST 异常")
    void helpKanJia_duplicateKeyException_convertsToFriendlyException() {
        try (MockedStatic<UserContext> userContextMock = mockStatic(UserContext.class)) {
            userContextMock.when(UserContext::getCurrentUser).thenReturn(mockUser());

            doReturn(mockActivity()).when(kanjiaActivityService).getById("act_1");
            when(memberService.getById("10001")).thenReturn(mockMember());
            when(kanjiaActivityGoodsService.getById("goods_1")).thenReturn(mockGoods());
            when(kanjiaActivityLogService.count(any(LambdaQueryWrapper.class))).thenReturn(0L);
            when(kanjiaActivityLogService.addKanJiaActivityLog(any(KanjiaActivityDTO.class)))
                    .thenThrow(new DuplicateKeyException("Duplicate entry"));

            assertThatThrownBy(() -> kanjiaActivityService.helpKanJia("act_1"))
                    .isInstanceOf(ServiceException.class)
                    .extracting("resultCode")
                    .isEqualTo(ResultCode.PROMOTION_LOG_EXIST);

            verify(rLock).unlock();
        }
    }
}
