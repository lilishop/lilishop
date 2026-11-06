package cn.lili.modules.payment.kit.plugin.wallet;

import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.service.RefundLogService;
import cn.lili.modules.wallet.entity.dto.MemberWalletUpdateDTO;
import cn.lili.modules.wallet.service.MemberWalletService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.test.util.ReflectionTestUtils;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 余额退款幂等测试：同一订单同一售后单号已退款成功时，不允许重复入账，
 * 覆盖订单取消双通道退款、退款重试等场景的防重保护
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
class WalletPluginRefundIdempotentTest {

    private WalletPlugin walletPlugin;
    private MemberWalletService memberWalletService;
    private RefundLogService refundLogService;

    /**
     * 初始化钱包插件的模拟依赖：会员钱包、退款日志与分布式锁。
     */
    @BeforeEach
    void setUp() {
        walletPlugin = new WalletPlugin();
        memberWalletService = mock(MemberWalletService.class);
        refundLogService = mock(RefundLogService.class);
        RedissonClient redissonClient = mock(RedissonClient.class);
        RLock lock = mock(RLock.class);
        when(redissonClient.getLock(any(String.class))).thenReturn(lock);
        ReflectionTestUtils.setField(walletPlugin, "memberWalletService", memberWalletService);
        ReflectionTestUtils.setField(walletPlugin, "refundLogService", refundLogService);
        ReflectionTestUtils.setField(walletPlugin, "redisson", redissonClient);
    }

    /**
     * 构造订单取消场景的退款日志。
     *
     * @return 退款日志
     */
    private RefundLog buildCancelRefundLog() {
        return RefundLog.builder()
                .isRefund(false)
                .totalAmount(666.0)
                .payPrice(666.0)
                .memberId("m1")
                .paymentName("WALLET")
                .afterSaleNo("ORDER_CANCEL")
                .orderSn("O2118532463188905984")
                .refundReason("订单取消")
                .build();
    }

    /**
     * 已有成功退款流水时，重复退款请求必须被忽略，余额不重复入账。
     */
    @Test
    void duplicateRefundIsIgnored() {
        when(refundLogService.count(any())).thenReturn(1L);

        walletPlugin.refund(buildCancelRefundLog());

        verify(memberWalletService, never()).increase(any(MemberWalletUpdateDTO.class));
        verify(refundLogService, never()).save(any(RefundLog.class));
    }

    /**
     * 无历史退款流水时正常退款入账并保存退款日志。
     */
    @Test
    void firstRefundIncreasesWallet() {
        when(refundLogService.count(any())).thenReturn(0L);
        RefundLog refundLog = buildCancelRefundLog();

        walletPlugin.refund(refundLog);

        verify(memberWalletService).increase(any(MemberWalletUpdateDTO.class));
        verify(refundLogService).save(refundLog);
        org.assertj.core.api.Assertions.assertThat(refundLog.getIsRefund()).isTrue();
    }

    /**
     * 订单取消退款的余额变动日志应展示“订单取消”，不得把占位售后单号 ORDER_CANCEL 直接暴露给客户。
     */
    @Test
    void orderCancelRefundDetailShowsCancelReason() {
        when(refundLogService.count(any())).thenReturn(0L);

        walletPlugin.refund(buildCancelRefundLog());

        ArgumentCaptor<MemberWalletUpdateDTO> captor = ArgumentCaptor.forClass(MemberWalletUpdateDTO.class);
        verify(memberWalletService).increase(captor.capture());
        org.assertj.core.api.Assertions.assertThat(captor.getValue().getDetail())
                .isEqualTo("订单[O2118532463188905984]，订单取消，退还金额[666.0]");
    }

    /**
     * 真实售后退款仍保留售后单号，便于客户按售后单核对退款来源。
     */
    @Test
    void afterSaleRefundDetailKeepsAfterSaleNo() {
        when(refundLogService.count(any())).thenReturn(0L);
        RefundLog refundLog = buildCancelRefundLog();
        refundLog.setAfterSaleNo("AS123456");

        walletPlugin.refund(refundLog);

        ArgumentCaptor<MemberWalletUpdateDTO> captor = ArgumentCaptor.forClass(MemberWalletUpdateDTO.class);
        verify(memberWalletService).increase(captor.capture());
        org.assertj.core.api.Assertions.assertThat(captor.getValue().getDetail())
                .isEqualTo("订单[O2118532463188905984]，售后单[AS123456]，退还金额[666.0]");
    }
}
