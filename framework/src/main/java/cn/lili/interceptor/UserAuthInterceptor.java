package cn.lili.interceptor;

import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.SecurityEnum;
import cn.lili.common.security.token.SecretKeyUtil;
import com.google.gson.Gson;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

@Slf4j
@Component
public class UserAuthInterceptor implements HandlerInterceptor {

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) {
        // 1. 从 Header 拿 Token
        String token = request.getHeader("accessToken");

        if (token == null) {
            log.info("请求token 为空");
            return false;
        }
        try {
            // 2. 全局唯一一次解密！将算力消耗降到最低！
            AuthUser user = parseToken(token);
            if (request.getHeader(SecurityEnum.UUID.getValue()) != null) {
                user.setUuid(request.getHeader(SecurityEnum.UUID.getValue()));
            }
            if (request.getHeader(SecurityEnum.INVITER.getValue()) != null) {
                user.setInviter(request.getHeader(SecurityEnum.INVITER.getValue()));
            }
            user.setAccessToken(token);
            // 3. 稳稳地放进咱们的高级盒子里
            UserContext.set(user);
        } catch (Exception e) {
            log.warn("无效的 Token，解析失败", e);
            // 这里可以根据业务决定是直接 return false 拦截，还是放行给特定的注解去校验
        }
        return true;
    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response, Object handler, Exception ex) {
        // 最关键的兜底！无论业务代码报不报错，请求结束必须清空 ThreadLocal！
        UserContext.clear();
    }

    private AuthUser parseToken(String accessToken) {
        //获取token的信息
        Claims claims
                = Jwts.parserBuilder()
                .setSigningKey(SecretKeyUtil.generalKeyByDecoders())
                .build()
                .parseClaimsJws(accessToken).getBody();
        //获取存储在claims中的用户信息
        String json = claims.get(SecurityEnum.USER_CONTEXT.getValue()).toString();
        return new Gson().fromJson(json, AuthUser.class);
    }
}