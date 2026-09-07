package cn.lili.controller.manager.distribution;

import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.service.DistributionOrderService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

/**
 * 分销失败且 MQ 不在时，reprocess 接口可补入账。
 *
 * @author Mike
 * @date 2026-09-07
 */
class DistributionOrderManagerControllerReprocessTest {

    private DistributionOrderManagerController controller;
    private DistributionOrderService distributionOrderService;

    @BeforeEach
    void setUp() {
        controller = new DistributionOrderManagerController();
        distributionOrderService = mock(DistributionOrderService.class);
        ReflectionTestUtils.setField(controller, "distributionOrderService", distributionOrderService);
    }

    /**
     * reprocess 接口委托 handlePaymentRewards 补入账。
     */
    @Test
    void reprocessRewardsDelegatesToHandlePaymentRewards() {
        // 管理端手动补入账
        ResultMessage<Object> result = controller.reprocessRewards("O001");

        assertThat(result.isSuccess()).isTrue();
        verify(distributionOrderService).handlePaymentRewards("O001");
    }
}
