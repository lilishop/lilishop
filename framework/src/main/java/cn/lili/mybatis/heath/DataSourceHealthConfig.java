package cn.lili.mybatis.heath;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.jdbc.health.DataSourceHealthIndicator;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.util.StringUtils;

import javax.sql.DataSource;

/**
 * 数据库检验工具
 * 适配 Spring Boot 3.x
 *
 * @author Chopper
 * @version v4.0
 * @since 2020/12/24 19:31
 */
@Configuration
public class DataSourceHealthConfig {

    @Value("${spring.datasource.dbcp2.validation-query:select 1}")
    private String defaultQuery;

    /**
     * 自定义数据源健康检查指示器
     * 使用指定的验证查询语句
     * 
     * @param dataSource 数据源
     * @return 数据源健康检查指示器
     */
    @Bean("dbHealthIndicator")
    public DataSourceHealthIndicator dataSourceHealthIndicator(DataSource dataSource) {
        // 创建带有自定义查询的健康检查指示器
        DataSourceHealthIndicator indicator = new DataSourceHealthIndicator(dataSource);
        
        // 如果配置了验证查询，则设置到指示器（Boot 3 API 不提供 getQuery）
        if (StringUtils.hasText(defaultQuery)) {
            indicator.setQuery(defaultQuery);
        }
        
        return indicator;
    }
}