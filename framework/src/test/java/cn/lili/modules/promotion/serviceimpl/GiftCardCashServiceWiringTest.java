package cn.lili.modules.promotion.serviceimpl;

import cn.lili.modules.promotion.service.GiftCardCashService;
import cn.lili.modules.promotion.service.PickupCardRuleService;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.test.util.ReflectionTestUtils;

import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

/**
 * 礼品卡与轻量提货规则服务装配测试，禁止循环引用并验证普通注入不依赖 Bean 注册顺序。
 * 两个服务使用真实实现，其余边界依赖使用模拟对象，不连接数据库、Redis 或消息队列。
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
class GiftCardCashServiceWiringTest {

    /**
     * 以两种注册顺序创建容器，验证礼品卡直接注入规则服务，不需要创建兑换及订单服务。
     */
    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    void wiresPickupRulesWithoutLazyProxy(boolean pickupFirst) {
        try (AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext()) {
            context.getDefaultListableBeanFactory().setAllowCircularReferences(false);
            Class<?>[] services = pickupFirst
                    ? new Class<?>[]{PickupCardRuleServiceImpl.class, GiftCardCashServiceImpl.class}
                    : new Class<?>[]{GiftCardCashServiceImpl.class, PickupCardRuleServiceImpl.class};
            Set<Class<?>> dependencies = new HashSet<>();
            for (Class<?> service : services) {
                for (Field field : service.getDeclaredFields()) {
                    if (field.isAnnotationPresent(Autowired.class) && field.getType() != PickupCardRuleService.class) {
                        dependencies.add(field.getType());
                    }
                }
            }
            dependencies.forEach(type -> registerMock(context, type));
            context.register(services);
            context.refresh();

            GiftCardCashService cashService = context.getBean(GiftCardCashService.class);
            PickupCardRuleService rules = context.getBean(PickupCardRuleService.class);
            assertThat(ReflectionTestUtils.getField(cashService, "pickupCardRuleService")).isSameAs(rules);
        }
    }

    /**
     * 按声明类型注册唯一的边界依赖，保留服务字段的真实 Spring 注入过程。
     */
    private static void registerMock(AnnotationConfigApplicationContext context, Class<?> type) {
        context.getBeanFactory().registerSingleton("mock_" + type.getName(), mock(type));
    }
}
