package cn.lili.modules.member.serviceimpl;

import cn.lili.modules.member.service.MemberBenefitDeliveryService;
import cn.lili.modules.member.service.MemberOrderPointService;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.mapper.OrderItemMapper;
import cn.lili.modules.order.aftersale.mapper.AfterSaleMapper;
import org.springframework.context.annotation.Configuration;
import org.springframework.transaction.annotation.EnableTransactionManagement;
import org.springframework.transaction.support.SimpleTransactionStatus;
import org.springframework.aop.support.AopUtils;

import cn.lili.modules.member.service.PaidMemberService;

import cn.lili.modules.member.service.MemberBenefitQueryService;

import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.mapper.MemberBenefitMapper;
import cn.lili.modules.member.mapper.MemberGradeMapper;
import cn.lili.modules.member.mapper.MemberGradeBenefitGrantMapper;
import cn.lili.modules.member.mapper.PaidMemberOrderMapper;
import cn.lili.modules.member.mapper.MemberBenefitDeliveryMapper;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.member.service.MemberGradeService;
import cn.lili.modules.member.service.MemberBenefitService;
import cn.lili.modules.member.listener.PaidMemberBenefitListener;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.modules.store.service.StoreService;
import cn.lili.modules.system.service.SettingService;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.transaction.PlatformTransactionManager;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * 禁用Spring循环引用后加载实际会员资格、权益查询和发放服务，验证不再依赖双方的Lazy代理。
 * 叶子存储及其他既有业务服务使用模拟对象，不代表完整应用或数据库集成启动验证。
 * @author Bulbasaur
 * @since 2026-11-05
 */
class MemberBenefitDependencyTest {
    /** 验证单向依赖装配、接口注入以及实现类事务注解经JDK代理仍会触发异常回滚。 */
    @Test void memberServicesInitializeWithCircularReferencesDisabled() {
        try (var context = new AnnotationConfigApplicationContext()) {
            context.getDefaultListableBeanFactory().setAllowCircularReferences(false);
            context.registerBean(MemberBenefitMapper.class, () -> mock(MemberBenefitMapper.class));
            context.registerBean(MemberGradeMapper.class, () -> mock(MemberGradeMapper.class));
            context.registerBean(MemberGradeBenefitGrantMapper.class, () -> mock(MemberGradeBenefitGrantMapper.class));
            context.registerBean(PaidMemberOrderMapper.class, () -> mock(PaidMemberOrderMapper.class));
            context.registerBean(MemberBenefitDeliveryMapper.class, () -> mock(MemberBenefitDeliveryMapper.class));
            context.registerBean(MemberService.class, () -> mock(MemberService.class));
            context.registerBean(MemberGradeService.class, () -> mock(MemberGradeService.class));
            context.registerBean(CouponService.class, () -> mock(CouponService.class));
            context.registerBean(MemberCouponService.class, () -> mock(MemberCouponService.class));
            context.registerBean(StoreService.class, () -> mock(StoreService.class));
            context.registerBean(GoodsSkuService.class, () -> mock(GoodsSkuService.class));
            context.registerBean(SettingService.class, () -> mock(SettingService.class));
            var transactions = mock(PlatformTransactionManager.class);
            when(transactions.getTransaction(any())).thenAnswer(call -> new SimpleTransactionStatus());
            context.registerBean(PlatformTransactionManager.class, () -> transactions);
            context.registerBean(cn.lili.modules.member.mapper.MemberPointsHistoryMapper.class, () -> mock(cn.lili.modules.member.mapper.MemberPointsHistoryMapper.class));
            context.registerBean(cn.lili.modules.promotion.service.PickupCardOrderService.class, () -> mock(cn.lili.modules.promotion.service.PickupCardOrderService.class));
            context.registerBean(OrderMapper.class, () -> mock(OrderMapper.class));
            context.registerBean(OrderItemMapper.class, () -> mock(OrderItemMapper.class));
            context.registerBean(AfterSaleMapper.class, () -> mock(AfterSaleMapper.class));
            context.register(TransactionConfiguration.class,MemberOrderPointServiceImpl.class);
            context.register(MemberBenefitServiceImpl.class,MemberBenefitQueryServiceImpl.class,
                    PaidMemberServiceImpl.class,MemberBenefitDeliveryServiceImpl.class,PaidMemberBenefitListener.class);
            context.refresh();
            assertThat(AopUtils.getTargetClass(context.getBean(MemberBenefitService.class))).isEqualTo(MemberBenefitServiceImpl.class);
            assertThat(context.getBean(PaidMemberService.class)).isNotNull();
            assertThat(context.getBean(MemberBenefitQueryService.class).listByIdList(null,true)).isEmpty();
            assertThat(AopUtils.isJdkDynamicProxy(context.getBean(PaidMemberService.class))).isTrue();
            assertThat(AopUtils.isJdkDynamicProxy(context.getBean(MemberOrderPointService.class))).isTrue();
            assertThatThrownBy(() -> context.getBean(PaidMemberService.class).cancel("member","PM-missing"))
                    .hasMessageContaining("仅未支付订单可以取消");
            assertThatThrownBy(() -> context.getBean(MemberOrderPointService.class).complete("missing"))
                    .hasMessage("订单不存在");
            verify(transactions,times(2)).rollback(any());
        }
    }
    /**
     * 启用基于接口的事务代理，验证实现类迁移后仍能识别原有事务注解。
     * @author Bulbasaur
     * @since 2026-11-05
     */
    @Configuration(proxyBeanMethods = false)
    @EnableTransactionManagement
    static class TransactionConfiguration { }
}
