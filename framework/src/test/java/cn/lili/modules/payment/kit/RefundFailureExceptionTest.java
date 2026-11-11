package cn.lili.modules.payment.kit;

import cn.lili.common.exception.ServiceException;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.kit.plugin.bank.BankTransferPlugin;
import cn.lili.modules.payment.kit.plugin.nopay.NotActuallyPaidPlugin;
import cn.lili.modules.payment.kit.plugin.wallet.WalletPlugin;
import cn.lili.modules.payment.service.RefundLogService;
import cn.lili.modules.wallet.entity.dto.MemberWalletUpdateDTO;
import cn.lili.modules.wallet.entity.enums.DepositServiceTypeEnum;
import cn.lili.modules.wallet.service.MemberWalletService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * 退款失败异常传播测试
 * 验证渠道发起退款失败时抛出业务异常阻断流程，不再静默捕获吞并异常
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class RefundFailureExceptionTest {

    @Test
    @DisplayName("余额退款失败时抛出ServiceException阻断流程")
    void walletRefundThrowsExceptionOnFailure() {
        WalletPlugin walletPlugin = new WalletPlugin();
        RedissonClient redisson = mock(RedissonClient.class);
        RLock lock = mock(RLock.class);
        when(redisson.getLock(anyString())).thenReturn(lock);

        RefundLogService refundLogService = mock(RefundLogService.class);
        when(refundLogService.count(any())).thenReturn(0L);

        MemberWalletService memberWalletService = mock(MemberWalletService.class);
        doThrow(new RuntimeException("数据库死锁")).when(memberWalletService).increase(any(MemberWalletUpdateDTO.class));

        ReflectionTestUtils.setField(walletPlugin, "redisson", redisson);
        ReflectionTestUtils.setField(walletPlugin, "refundLogService", refundLogService);
        ReflectionTestUtils.setField(walletPlugin, "memberWalletService", memberWalletService);

        RefundLog refundLog = RefundLog.builder()
                .isRefund(false)
                .totalAmount(100.0)
                .memberId("M_001")
                .orderSn("O_001")
                .afterSaleNo("AS_001")
                .build();

        assertThatThrownBy(() -> walletPlugin.refund(refundLog))
                .isInstanceOf(ServiceException.class)
                .hasMessageContaining("余额退款失败");

        verify(lock).unlock();
    }

    @Test
    @DisplayName("线下转账退款保存失败时抛出ServiceException")
    void bankTransferRefundThrowsExceptionOnSaveFailure() {
        BankTransferPlugin plugin = new BankTransferPlugin();
        RefundLogService refundLogService = mock(RefundLogService.class);
        doThrow(new RuntimeException("数据库写入失败")).when(refundLogService).save(any(RefundLog.class));
        ReflectionTestUtils.setField(plugin, "refundLogService", refundLogService);

        RefundLog refundLog = RefundLog.builder().build();

        assertThatThrownBy(() -> plugin.refund(refundLog))
                .isInstanceOf(ServiceException.class)
                .hasMessageContaining("线下退款记录失败");
    }

    @Test
    @DisplayName("零元未实际支付退款保存失败时抛出ServiceException")
    void notActuallyPaidRefundThrowsExceptionOnSaveFailure() {
        NotActuallyPaidPlugin plugin = new NotActuallyPaidPlugin();
        RefundLogService refundLogService = mock(RefundLogService.class);
        doThrow(new RuntimeException("数据库写入失败")).when(refundLogService).save(any(RefundLog.class));
        ReflectionTestUtils.setField(plugin, "refundLogService", refundLogService);

        RefundLog refundLog = RefundLog.builder().build();

        assertThatThrownBy(() -> plugin.refund(refundLog))
                .isInstanceOf(ServiceException.class)
                .hasMessageContaining("未实际支付订单退款记录失败");
    }
}
