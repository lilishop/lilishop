package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.appointment.support.AppointmentGoodsMetadataSupport;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.goods.support.*;
import cn.lili.modules.promotion.serviceimpl.CouponServiceImpl;
import cn.lili.modules.promotion.serviceimpl.MemberCouponServiceImpl;
import cn.lili.modules.promotion.serviceimpl.PromotionGoodsServiceImpl;
import cn.lili.modules.promotion.serviceimpl.PromotionServiceImpl;
import cn.lili.modules.promotion.support.SkuPromotionQuerySupport;
import cn.lili.modules.search.serviceimpl.GoodsIndexServiceImpl;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

/**
 * 商品与SKU去除延迟注入的装配回归测试，使用真实核心服务及支持类，并启用事务代理和禁止循环引用。
 * 外围存储与其他模块边界使用模拟对象，不代表完整应用启动或数据库集成验证。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class GoodsServicesDependencyTest {

    /**
     * 在两种注册顺序下验证核心组件无需Lazy即可装配，不依赖偶然的Bean创建顺序。
     *
     * @param reverse 是否反转真实组件注册顺序
     */
    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void initializesWithoutLazyOrCircularReferences(boolean reverse) {
        List<Class<?>> targets = new ArrayList<>(List.of(GoodsServiceImpl.class, GoodsSkuServiceImpl.class,
                GoodsVOAssembler.class, GoodsMarketAuditManager.class, GoodsSkuStockManager.class,
                GoodsSkuStockExcelSupport.class, GoodsSkuSpecAssembler.class, GoodsSkuReadSupport.class,
                GoodsStockAggregateSupport.class, CardKeyPoolSupport.class, AppointmentGoodsMetadataSupport.class,
                GoodsIndexServiceImpl.class, PromotionGoodsServiceImpl.class, CouponServiceImpl.class,
                MemberCouponServiceImpl.class, PromotionServiceImpl.class,
                SkuPromotionQuerySupport.class, CategoryServiceImpl.class,
                ParametersServiceImpl.class, CategoryParameterServiceImpl.class));
        if (reverse) {
            Collections.reverse(targets);
        }
        try (var context = new AnnotationConfigApplicationContext()) {
            context.getDefaultListableBeanFactory().setAllowCircularReferences(false);
            Set<Class<?>> dependencies = new HashSet<>();
            dependencies.add(PlatformTransactionManager.class);
            for (Class<?> target : targets) {
                for (var field : target.getDeclaredFields()) {
                    if (field.isAnnotationPresent(Autowired.class)) {
                        if (field.getType() == ObjectProvider.class) {
                            dependencies.add((Class<?>) ((ParameterizedType) field.getGenericType()).getActualTypeArguments()[0]);
                        } else {
                            dependencies.add(field.getType());
                        }
                    }
                }
                if (target.getDeclaredConstructors().length == 1) {
                    dependencies.addAll(Arrays.asList(target.getDeclaredConstructors()[0].getParameterTypes()));
                }
                if (target.getGenericSuperclass() instanceof ParameterizedType parent) {
                    dependencies.add((Class<?>) parent.getActualTypeArguments()[0]);
                }
            }
            dependencies.removeIf(type -> type == ApplicationEventPublisher.class
                    || targets.stream().anyMatch(type::isAssignableFrom));
            for (Class<?> dependency : dependencies) {
                context.getBeanFactory().registerSingleton("mock_" + dependency.getName(), mock(dependency));
            }
            context.register(TransactionConfiguration.class);
            context.register(targets.toArray(Class<?>[]::new));
            context.refresh();
            assertThat(AopUtils.getTargetClass(context.getBean(GoodsService.class))).isEqualTo(GoodsServiceImpl.class);
            assertThat(AopUtils.getTargetClass(context.getBean(GoodsSkuService.class))).isEqualTo(GoodsSkuServiceImpl.class);
            for (Class<?> target : List.of(GoodsServiceImpl.class, GoodsSkuServiceImpl.class, GoodsVOAssembler.class,
                    GoodsMarketAuditManager.class, GoodsSkuStockManager.class, GoodsSkuStockExcelSupport.class,
                    GoodsSkuReadSupport.class, GoodsStockAggregateSupport.class, CardKeyPoolSupport.class,
                    AppointmentGoodsMetadataSupport.class, GoodsIndexServiceImpl.class, SkuPromotionQuerySupport.class,
                    ParametersServiceImpl.class)) {
                assertThat(target.isAnnotationPresent(Lazy.class)).isFalse();
                assertThat(Arrays.stream(target.getDeclaredFields()).filter(f -> f.isAnnotationPresent(Lazy.class)))
                        .as(target.getSimpleName()).isEmpty();
            }
        }
    }

    /**
     * 在小型装配容器启用与生产一致的事务代理，不创建真实数据库事务。
     *
     * @author Bulbasaur
     * @since 2026-11-13
     */
    @Configuration(proxyBeanMethods = false)
    @EnableTransactionManagement(proxyTargetClass = true)
    static class TransactionConfiguration {
    }
}
