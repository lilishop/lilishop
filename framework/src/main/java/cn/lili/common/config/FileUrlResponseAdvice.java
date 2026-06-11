package cn.lili.common.config;

import cn.lili.modules.file.service.FileUrlService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.MethodParameter;
import org.springframework.http.MediaType;
import org.springframework.http.converter.HttpMessageConverter;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice;

/**
 * 接口输出前统一处理文件访问地址，避免商品、文章、装修等模块分别硬编码 CDN 逻辑。
 */
@ControllerAdvice
public class FileUrlResponseAdvice implements ResponseBodyAdvice<Object> {

    @Autowired
    private FileUrlService fileUrlService;

    @Override
    public boolean supports(MethodParameter returnType, Class<? extends HttpMessageConverter<?>> converterType) {
        return true;
    }

    @Override
    public Object beforeBodyWrite(Object body, MethodParameter returnType, MediaType selectedContentType,
                                  Class<? extends HttpMessageConverter<?>> selectedConverterType,
                                  ServerHttpRequest request, ServerHttpResponse response) {
        return fileUrlService.rewriteResponse(body);
    }
}
