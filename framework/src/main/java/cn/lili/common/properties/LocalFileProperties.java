package cn.lili.common.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * 本地文件访问配置。
 */
@Data
@Configuration
@ConfigurationProperties(prefix = "lili.local-file")
public class LocalFileProperties {

    /**
     * 本地文件保存目录
     */
    private String path = "data/uploads";

    /**
     * 对外访问路径前缀
     */
    private String urlPrefix = "/files";
}
