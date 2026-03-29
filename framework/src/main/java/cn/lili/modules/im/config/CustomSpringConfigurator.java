package cn.lili.modules.im.config;


import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.SecurityEnum;
import cn.lili.common.security.token.SecretKeyUtil;
import com.google.gson.Gson;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import javax.websocket.HandshakeResponse;
import javax.websocket.server.HandshakeRequest;
import javax.websocket.server.ServerEndpointConfig;
import java.util.List;

/**
 * CustomSpringConfigurator
 *
 * @author Chopper
 * @version v1.0
 * 2021-12-31 11:53
 */
public class CustomSpringConfigurator extends ServerEndpointConfig.Configurator implements ApplicationContextAware {

    /**
     * Spring application context.
     */
    private static volatile BeanFactory context;

    @Override
    public <T> T getEndpointInstance(Class<T> clazz) throws InstantiationException {
        return context.getBean(clazz);
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        CustomSpringConfigurator.context = applicationContext;
    }

    @Override
    public void modifyHandshake(ServerEndpointConfig sec, HandshakeRequest request, HandshakeResponse response) {
        // 1. 从子协议头部提取前端伪装的 Token
        List<String> subProtocols = request.getHeaders().get("Sec-WebSocket-Protocol");
        if (subProtocols != null && !subProtocols.isEmpty()) {
            String token = subProtocols.get(0);
            try {
                // 2. 解析 JWT（只在连接握手时解析 1 次！）
                AuthUser user = parseToken(token);
                // 3. 👑 核心绝杀：把用户信息塞进这个 WebSocket 的原生 Session 属性里！
                sec.getUserProperties().put("CURRENT_USER", user);

                // 告诉前端我们接受了这个子协议
                response.getHeaders().put("Sec-WebSocket-Protocol", subProtocols);
            } catch (Exception e) {
                throw new RuntimeException("WebSocket 鉴权失败！"); // 直接拒绝连接
            }
        }
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
