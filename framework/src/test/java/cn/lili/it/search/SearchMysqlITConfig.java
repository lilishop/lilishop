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

    @Bean
    DataSource dataSource() {
        HikariDataSource dataSource = new HikariDataSource();
        dataSource.setJdbcUrl(MysqlITSupport.jdbcUrl());
        dataSource.setUsername(MysqlITSupport.username());
        dataSource.setPassword(MysqlITSupport.password());
        dataSource.setDriverClassName("com.mysql.cj.jdbc.Driver");
        return dataSource;
    }

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

    @Bean
    SqlSessionTemplate sqlSessionTemplate(SqlSessionFactory sqlSessionFactory) {
        return new SqlSessionTemplate(sqlSessionFactory);
    }

    @Bean
    PlatformTransactionManager transactionManager(DataSource dataSource) {
        return new DataSourceTransactionManager(dataSource);
    }

    @Bean
    MapperFactoryBean<GoodsMapper> goodsMapper(SqlSessionFactory sqlSessionFactory) {
        return mapper(GoodsMapper.class, sqlSessionFactory);
    }

    @Bean
    MapperFactoryBean<GoodsSkuMapper> goodsSkuMapper(SqlSessionFactory sqlSessionFactory) {
        return mapper(GoodsSkuMapper.class, sqlSessionFactory);
    }

    @Bean
    MapperFactoryBean<GoodsSearchParamMapper> goodsSearchParamMapper(SqlSessionFactory sqlSessionFactory) {
        return mapper(GoodsSearchParamMapper.class, sqlSessionFactory);
    }

    @Bean
    Cache<RebuildStatus> cache() {
        return new ITCache();
    }

    @Bean
    RedissonClient redissonClient() throws InterruptedException {
        RLock lock = mock(RLock.class);
        when(lock.tryLock(anyLong(), anyLong(), any(TimeUnit.class))).thenReturn(true);
        when(lock.isHeldByCurrentThread()).thenReturn(true);
        RedissonClient client = mock(RedissonClient.class);
        when(client.getLock(anyString())).thenReturn(lock);
        return client;
    }

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

    @Bean
    PromotionService promotionService() {
        PromotionService promotionService = mock(PromotionService.class);
        when(promotionService.getGoodsSkuPromotionMap(any(), any())).thenReturn(java.util.Collections.emptyMap());
        return promotionService;
    }

    @Bean
    PromotionGoodsService promotionGoodsService() {
        return mock(PromotionGoodsService.class);
    }

    @Bean
    GoodsSkuService goodsSkuService(GoodsSkuMapper goodsSkuMapper) {
        GoodsSkuService goodsSkuService = mock(GoodsSkuService.class);
        when(goodsSkuService.getGoodsSkuDTOByPage(any(), any())).thenAnswer(invocation ->
                goodsSkuMapper.queryByParams(invocation.getArgument(0), invocation.getArgument(1)));
        return goodsSkuService;
    }

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

    private static <T> MapperFactoryBean<T> mapper(Class<T> type, SqlSessionFactory sqlSessionFactory) {
        MapperFactoryBean<T> factoryBean = new MapperFactoryBean<>(type);
        factoryBean.setSqlSessionFactory(sqlSessionFactory);
        return factoryBean;
    }
}
