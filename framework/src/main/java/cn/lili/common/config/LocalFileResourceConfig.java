package cn.lili.common.config;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.properties.LocalFileProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import java.net.URI;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * 本地上传文件静态访问映射。
 */
@Configuration
public class LocalFileResourceConfig implements WebMvcConfigurer {

    @Autowired
    private LocalFileProperties localFileProperties;

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        String urlPrefix = CharSequenceUtil.blankToDefault(localFileProperties.getUrlPrefix(), "/files");
        String handler = CharSequenceUtil.removeSuffix(toRequestPath(urlPrefix), "/") + "/**";
        Path root = Paths.get(localFileProperties.getPath()).toAbsolutePath().normalize();
        registry.addResourceHandler(handler).addResourceLocations(CharSequenceUtil.addSuffixIfNot(root.toUri().toString(), "/"));
    }

    private String toRequestPath(String urlPrefix) {
        try {
            URI uri = URI.create(urlPrefix);
            if (uri.isAbsolute() && CharSequenceUtil.isNotBlank(uri.getPath())) {
                return uri.getPath();
            }
        } catch (Exception ignored) {
            // 配置不是完整 URI 时按普通路径处理。
        }
        return urlPrefix.startsWith("/") ? urlPrefix : "/" + urlPrefix;
    }
}
