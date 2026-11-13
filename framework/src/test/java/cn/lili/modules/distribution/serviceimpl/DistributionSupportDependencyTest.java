package cn.lili.modules.distribution.serviceimpl;

import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.support.DistributionMemberAdminSupport;
import cn.lili.modules.distribution.support.DistributionMemberExportSupport;
import cn.lili.modules.distribution.support.DistributionPromotionSupport;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.UnsatisfiedDependencyException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import java.util.HashSet;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.mock;

/**
 * 分销管理支持类装配回归测试，验证必需依赖和禁止循环引用时的装配；外围服务使用模拟对象，不连接数据库。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class DistributionSupportDependencyTest {

    /** 验证分销服务和真实管理、导出、推广支持类能够以单向依赖装配。 */
    @Test
    void initializesWithRequiredSupports() {
        try (var context = contextWithLeafMocks()) {
            context.register(DistributionMemberAdminSupport.class, DistributionMemberExportSupport.class,
                    DistributionPromotionSupport.class);
            context.refresh();
            assertThat(context.getBean(DistributionService.class)).isInstanceOf(DistributionServiceImpl.class);
        }
    }

    /** 验证管理支持类缺失时启动失败，禁止在业务执行中手动创建实例兜底。 */
    @Test
    void missingAdminSupportPreventsInitialization() {
        try (var context = contextWithLeafMocks()) {
            context.register(DistributionMemberExportSupport.class, DistributionPromotionSupport.class);
            assertThatThrownBy(context::refresh).isInstanceOf(UnsatisfiedDependencyException.class)
                    .hasMessageContaining("distributionMemberAdminSupport");
        }
    }

    /**
     * 为分销服务和三个支持类注册外围依赖，保留支持类间真实装配并禁止循环引用。
     *
     * @return 尚未刷新的测试容器，调用方负责关闭
     */
    private AnnotationConfigApplicationContext contextWithLeafMocks() {
        var context = new AnnotationConfigApplicationContext();
        context.getDefaultListableBeanFactory().setAllowCircularReferences(false);
        Set<Class<?>> targets = Set.of(DistributionServiceImpl.class, DistributionMemberAdminSupport.class,
                DistributionMemberExportSupport.class, DistributionPromotionSupport.class);
        Set<Class<?>> leaves = new HashSet<>();
        leaves.add(DistributionMapper.class);
        for (Class<?> target : targets) {
            for (var field : target.getDeclaredFields()) {
                if (field.isAnnotationPresent(Autowired.class) && !targets.contains(field.getType())) {
                    leaves.add(field.getType());
                }
            }
        }
        for (Class<?> leaf : leaves) {
            context.getBeanFactory().registerSingleton("mock_" + leaf.getName(), mock(leaf));
        }
        context.register(DistributionServiceImpl.class);
        return context;
    }
}
