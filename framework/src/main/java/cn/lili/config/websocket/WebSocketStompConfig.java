package cn.lili.config.websocket;

import org.springframework.context.annotation.Configuration;
import org.springframework.messaging.simp.config.MessageBrokerRegistry;
import org.springframework.web.socket.config.annotation.EnableWebSocketMessageBroker;
import org.springframework.web.socket.config.annotation.StompEndpointRegistry;
import org.springframework.web.socket.config.annotation.WebSocketMessageBrokerConfigurer;

/**
 * @author Chopper
 */
@Configuration
@EnableWebSocketMessageBroker
public class WebSocketStompConfig implements WebSocketMessageBrokerConfigurer {

    /**
     * 注册stomp端点
     * @param registry
     */
    @Override
    public void registerStompEndpoints(StompEndpointRegistry registry) {

        //允许使用socketJs方式访问 即可通过http://IP:PORT/manager/ws来和服务端websocket连接
        registry.addEndpoint("/ws").setAllowedOrigins("*").withSockJS();
    }

    /**
     * 配置信息代理
     * @param registry
     */
    @Override
    public void configureMessageBroker(MessageBrokerRegistry registry) {

        //订阅Broker名称 user点对点 topic广播即群发
        registry.enableSimpleBroker("/user","/topic");
        //全局(客户端)使用的消息前缀
        registry.setApplicationDestinationPrefixes("/app");
        //点对点使用的前缀 无需配置 默认/user
        registry.setUserDestinationPrefix("/user");
    }
}
