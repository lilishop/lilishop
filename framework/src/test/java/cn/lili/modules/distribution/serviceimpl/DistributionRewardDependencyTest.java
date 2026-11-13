package cn.lili.modules.distribution.serviceimpl;

import cn.lili.modules.distribution.mapper.DistributionOrderMapper;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.distribution.service.DistributionRewardService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.service.DistributionWalletAccountService;
import cn.lili.modules.distribution.support.DistributionOrderPersistSupport;
import cn.lili.modules.distribution.support.DistributionRewardContextSupport;
import cn.lili.modules.distribution.support.DistributionRewardWalletSupport;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.promotion.service.PickupCardOrderService;
import cn.lili.modules.system.service.SettingService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.UnsatisfiedDependencyException;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.mock;

/**
 * 分销奖励及持久化支持类装配测试，验证必需依赖和分销订单服务单向调用，不启动完整应用或连接数据库。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class DistributionRewardDependencyTest {

    /** 验证禁用循环引用时，分销订单服务、奖励服务和三个真实支持类能够一起装配。 */
    @Test
    void initializesWithCircularReferencesDisabled() {
        try (var context = contextWithLeafDependencies()) {
            context.registerBean(OrderService.class, () -> mock(OrderService.class));
            context.refresh();
            assertThat(context.getBean(DistributionRewardService.class))
                    .isInstanceOf(DistributionRewardServiceImpl.class);
            assertThat(context.getBean(DistributionOrderService.class))
                    .isInstanceOf(DistributionOrderServiceImpl.class);
            assertThat(context.getBean(DistributionOrderPersistSupport.class)).isNotNull();
        }
    }

    /** 验证缺少主订单查询服务时启动失败，禁止再以空结果掩盖依赖缺失。 */
    @Test
    void missingOrderServicePreventsInitialization() {
        try (var context = contextWithLeafDependencies()) {
            assertThatThrownBy(context::refresh).isInstanceOf(UnsatisfiedDependencyException.class);
        }
    }

    /**
     * 注册真实分销订单与奖励服务及支持类，其余外围服务使用模拟对象。
     * 主订单服务由各场景决定是否提供；不代表完整订单模块依赖图已验收。
     *
     * @return 尚未刷新的测试容器，由调用方关闭
     */
    private AnnotationConfigApplicationContext contextWithLeafDependencies() {
        var context = new AnnotationConfigApplicationContext();
        context.getDefaultListableBeanFactory().setAllowCircularReferences(false);
        context.registerBean(DistributionOrderMapper.class, () -> mock(DistributionOrderMapper.class));
        context.registerBean(OrderItemService.class, () -> mock(OrderItemService.class));
        context.registerBean(PickupCardOrderService.class, () -> mock(PickupCardOrderService.class));
        context.registerBean(DistributionService.class, () -> mock(DistributionService.class));
        context.registerBean(DistributionGradeService.class, () -> mock(DistributionGradeService.class));
        context.registerBean(DistributionGoodsService.class, () -> mock(DistributionGoodsService.class));
        context.registerBean(DistributionCustomerRelationService.class,
                () -> mock(DistributionCustomerRelationService.class));
        context.registerBean(DistributionWalletAccountService.class,
                () -> mock(DistributionWalletAccountService.class));
        context.registerBean(SettingService.class, () -> mock(SettingService.class));
        context.register(DistributionRewardServiceImpl.class, DistributionOrderServiceImpl.class,
                DistributionOrderPersistSupport.class, DistributionRewardContextSupport.class,
                DistributionRewardWalletSupport.class);
        return context;
    }
}
