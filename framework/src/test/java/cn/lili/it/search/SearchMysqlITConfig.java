package cn.lili.it.search;

import cn.lili.cache.Cache;
import cn.lili.modules.goods.config.GoodsSearchParamRebuildConfig;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dto.RebuildStatus;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.goods.mapper.GoodsSearchParamMapper;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.goods.serviceimpl.GoodsSearchParamServiceImpl;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.modules.search.serviceimpl.GoodsSearchServiceImpl;
import cn.lili.mybatis.mybatisplus.MyMetaObjectHandler;
import com.baomidou.mybatisplus.annotation.DbType;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.config.GlobalConfig;
import com.baomidou.mybatisplus.extension.plugins.MybatisPlusInterceptor;
import com.baomidou.mybatisplus.extension.plugins.inner.PaginationInnerInterceptor;
import com.baomidou.mybatisplus.extension.spring.MybatisSqlSessionFactoryBean;
import com.zaxxer.hikari.HikariDataSource;
import org.apache.ibatis.session.SqlSessionFactory;
import org.apache.ibatis.type.JdbcType;
import org.mybatis.spring.SqlSessionTemplate;
import org.mybatis.spring.mapper.MapperFactoryBean;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.io.support.PathMatchingResourcePatternResolver;
import org.springframework.jdbc.datasource.DataSourceTransactionManager;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import javax.sql.DataSource;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 搜索 IT 最小 Spring 上下文：真实 MyBatis-Plus + MySQL，Redis 用内存替身。
 *
 * @author Mike
 * @date 2026-09-03
 */
@Configuration
@EnableTransactionManagement
@Import({GoodsSearchParamServiceImpl.class, GoodsSearchParamRebuildConfig.class, MyMetaObjectHandler.class})
class SearchMysqlITConfig {

    /**
     * 配置连接测试MySQL容器的数据源。
     *
     * @return 测试数据源
     */
    @Bean
    DataSource dataSource() {
        HikariDataSource dataSource = new HikariDataSource();
        dataSource.setJdbcUrl(MysqlITSupport.jdbcUrl());
        dataSource.setUsername(MysqlITSupport.username());
        dataSource.setPassword(MysqlITSupport.password());
        dataSource.setDriverClassName("com.mysql.cj.jdbc.Driver");
        return dataSource;
    }

    /**
     * 创建启用MyBatis-Plus映射及元数据处理的测试会话工厂。
     *
     * @param dataSource        测试数据源
     * @param metaObjectHandler 字段自动填充处理器
     * @return 启用MyBatis-Plus映射及元数据处理的测试会话工厂
     */
    @Bean
    SqlSessionFactory sqlSessionFactory(DataSource dataSource, MyMetaObjectHandler metaObjectHandler) throws Exception {
        MybatisSqlSessionFactoryBean factoryBean = new MybatisSqlSessionFactoryBean();
        factoryBean.setDataSource(dataSource);
        factoryBean.setMapperLocations(new PathMatchingResourcePatternResolver()
                .getResources("classpath*:mapper/**/*.xml"));
        MybatisPlusInterceptor interceptor = new MybatisPlusInterceptor();
        interceptor.addInnerInterceptor(new PaginationInnerInterceptor(DbType.MYSQL));
        factoryBean.setPlugins(interceptor);
        GlobalConfig globalConfig = new GlobalConfig();
        globalConfig.setMetaObjectHandler(metaObjectHandler);
        factoryBean.setGlobalConfig(globalConfig);
        MybatisConfiguration configuration = new MybatisConfiguration();
        configuration.setMapUnderscoreToCamelCase(true);
        configuration.setJdbcTypeForNull(JdbcType.NULL);
        factoryBean.setConfiguration(configuration);
        return factoryBean.getObject();
    }

    /**
     * 创建测试使用的MyBatis会话模板。
     *
     * @param sqlSessionFactory MyBatis会话工厂
     * @return 测试使用的MyBatis会话模板
     */
    @Bean
    SqlSessionTemplate sqlSessionTemplate(SqlSessionFactory sqlSessionFactory) {
        return new SqlSessionTemplate(sqlSessionFactory);
    }

    /**
     * 创建基于测试数据源的事务管理器。
     *
     * @param dataSource 测试数据源
     * @return 基于测试数据源的事务管理器
     */
    @Bean
    PlatformTransactionManager transactionManager(DataSource dataSource) {
        return new DataSourceTransactionManager(dataSource);
    }

    /**
     * 注册商品Mapper测试Bean。
     *
     * @param sqlSessionFactory MyBatis会话工厂
     * @return 商品Mapper测试Bean
     */
    @Bean
    MapperFactoryBean<GoodsMapper> goodsMapper(SqlSessionFactory sqlSessionFactory) {
        return mapper(GoodsMapper.class, sqlSessionFactory);
    }

    /**
     * 注册商品SKU Mapper测试Bean。
     *
     * @param sqlSessionFactory MyBatis会话工厂
     * @return 商品SKU Mapper测试Bean
     */
    @Bean
    MapperFactoryBean<GoodsSkuMapper> goodsSkuMapper(SqlSessionFactory sqlSessionFactory) {
        return mapper(GoodsSkuMapper.class, sqlSessionFactory);
    }

    /**
     * 注册商品参数倒排Mapper测试Bean。
     *
     * @param sqlSessionFactory MyBatis会话工厂
     * @return 商品参数倒排Mapper测试Bean
     */
    @Bean
    MapperFactoryBean<GoodsSearchParamMapper> goodsSearchParamMapper(SqlSessionFactory sqlSessionFactory) {
        return mapper(GoodsSearchParamMapper.class, sqlSessionFactory);
    }

    /**
     * 提供不依赖Redis的重建进度内存缓存。
     *
     * @return 不依赖Redis的重建进度内存缓存
     */
    @Bean
    Cache<RebuildStatus> cache() {
        return new ITCache();
    }

    /**
     * 提供能够获取重建锁的Redisson测试替身。
     *
     * @return 能够获取重建锁的Redisson测试替身
     */
    @Bean
    RedissonClient redissonClient() throws InterruptedException {
        RLock lock = mock(RLock.class);
        when(lock.tryLock(anyLong(), anyLong(), any(TimeUnit.class))).thenReturn(true);
        when(lock.isHeldByCurrentThread()).thenReturn(true);
        RedissonClient client = mock(RedissonClient.class);
        when(client.getLock(anyString())).thenReturn(lock);
        return client;
    }

    /**
     * 提供按层级查询分类的服务测试替身。
     *
     * @return 按层级查询分类的服务测试替身
     */
    @Bean
    CategoryService categoryService() {
        CategoryService categoryService = mock(CategoryService.class);
        when(categoryService.listByIdsOrderByLevel(any())).thenAnswer(invocation -> {
            List<String> ids = invocation.getArgument(0);
            List<Category> categories = new ArrayList<>();
            for (String id : ids) {
                Category category = new Category();
                category.setId(id);
                category.setName("cat-" + id);
                categories.add(category);
            }
            return categories;
        });
        return categoryService;
    }

    /**
     * 提供商品搜索使用的促销服务测试替身。
     *
     * @return 商品搜索使用的促销服务测试替身
     */
    @Bean
    PromotionService promotionService() {
        PromotionService promotionService = mock(PromotionService.class);
        when(promotionService.getGoodsSkuPromotionMap(any(), any())).thenReturn(java.util.Collections.emptyMap());
        return promotionService;
    }

    /**
     * 提供促销商品关联服务测试替身。
     *
     * @return 促销商品关联服务测试替身
     */
    @Bean
    PromotionGoodsService promotionGoodsService() {
        return mock(PromotionGoodsService.class);
    }

    /**
     * 组装使用测试Mapper的SKU服务。
     *
     * @param goodsSkuMapper 商品SKU Mapper
     * @return 使用测试Mapper的SKU服务
     */
    @Bean
    GoodsSkuService goodsSkuService(GoodsSkuMapper goodsSkuMapper) {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        when(goodsSkuService.getGoodsSkuDTOByPage(any(), any())).thenAnswer(invocation ->
                goodsSkuMapper.queryByParams(invocation.getArgument(0), invocation.getArgument(1)));
        return goodsSkuService;
    }

    /**
     * 组装使用测试依赖的商品搜索服务。
     *
     * @param goodsSkuService        商品SKU服务
     * @param goodsSkuMapper         商品SKU Mapper
     * @param goodsSearchParamMapper 商品参数Mapper
     * @param categoryService        分类服务
     * @param promotionService       促销服务
     * @param promotionGoodsService  促销商品服务
     * @return 使用测试依赖的商品搜索服务
     */
    @Bean
    GoodsSearchServiceImpl goodsSearchService(GoodsSkuService goodsSkuService,
                                              GoodsSkuMapper goodsSkuMapper,
                                              GoodsSearchParamMapper goodsSearchParamMapper,
                                              CategoryService categoryService,
                                              PromotionService promotionService,
                                              PromotionGoodsService promotionGoodsService) {
        GoodsSearchServiceImpl service = new GoodsSearchServiceImpl(goodsSkuService);
        ReflectionTestUtils.setField(service, "goodsSkuMapper", goodsSkuMapper);
        ReflectionTestUtils.setField(service, "goodsSearchParamMapper", goodsSearchParamMapper);
        ReflectionTestUtils.setField(service, "categoryService", categoryService);
        ReflectionTestUtils.setField(service, "promotionService", promotionService);
        ReflectionTestUtils.setField(service, "promotionGoodsService", promotionGoodsService);
        return service;
    }

    /**
     * 创建指定Mapper接口对应的测试工厂Bean。
     *
     * @param type              类型
     * @param sqlSessionFactory MyBatis会话工厂
     * @return 指定Mapper接口对应的测试工厂Bean
     */
    private static <T> MapperFactoryBean<T> mapper(Class<T> type, SqlSessionFactory sqlSessionFactory) {
        MapperFactoryBean<T> factoryBean = new MapperFactoryBean<>(type);
        factoryBean.setSqlSessionFactory(sqlSessionFactory);
        return factoryBean;
    }
}
