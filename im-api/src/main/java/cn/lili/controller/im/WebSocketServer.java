package cn.lili.controller.im;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.im.config.CustomSpringConfigurator;
import cn.lili.modules.im.entity.dos.ImMessage;
import cn.lili.modules.im.entity.enums.MessageResultType;
import cn.lili.modules.im.entity.vo.MessageOperation;
import cn.lili.modules.im.entity.vo.MessageVO;
import cn.lili.modules.im.service.ImMessageService;
import cn.lili.modules.im.service.ImTalkService;
import tools.jackson.databind.json.JsonMapper;
import jakarta.websocket.*;
import jakarta.websocket.server.PathParam;
import jakarta.websocket.server.ServerEndpoint;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author liushuai
 */
@Component
@ServerEndpoint(value = "/lili/webSocket/{accessToken}", configurator = CustomSpringConfigurator.class)
@Scope("prototype")
@Slf4j
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class WebSocketServer {
    /**
     * 在线人数 PS 注意，只能单节点，如果多节点部署需要自行寻找方案
     */
    private static ConcurrentHashMap<String, Session> sessionPools = new ConcurrentHashMap<>();
    /**
     * 消息服务
     */
    private final ImMessageService imMessageService;
    private final ImTalkService imTalkService;
    private final Cache cache;
    private final JsonMapper jsonMapper;

    /**
     * 建立连接
     *
     * @param session
     */
    @OnOpen
    public void onOpen(@PathParam("accessToken") String accessToken, Session session) {

        AuthUser authUser = UserContext.getAuthUser(cache, accessToken);

        String sessionId = UserEnums.STORE.equals(authUser.getRole()) ? authUser.getStoreId() : authUser.getId();
        //如果已有会话，则进行下线提醒。
        if (sessionPools.containsKey(sessionId)) {
            log.info("用户重复登陆，旧用户下线");
            Session oldSession = sessionPools.get(sessionId);
            sendMessage(oldSession,
                MessageVO.builder().messageResultType(MessageResultType.OFFLINE).result("用户异地登陆").build());
            try {
                oldSession.close();
            } catch (Exception e) {
                log.error("关闭旧会话异常", e);
            }
        }
        sessionPools.put(sessionId, session);
        log.info("用户建立连接，sessionId: {}", sessionId);
    }

    /**
     * 关闭连接
     */
    @OnClose
    public void onClose(@PathParam("accessToken") String accessToken) {
        AuthUser authUser = UserContext.getAuthUser(cache, accessToken);
        String sessionId = UserEnums.STORE.equals(authUser.getRole()) ? authUser.getStoreId() : authUser.getId();
        log.info("用户断开连接:{}", JSONUtil.toJsonStr(authUser));
        sessionPools.remove(sessionId);
    }

    /**
     * 发送消息
     *
     * @param msg
     * @throws IOException
     */
    @OnMessage
    public void onMessage(@PathParam("accessToken") String accessToken, String msg) {
        log.info("发送消息：{}", msg);
        try {
            MessageOperation messageOperation = jsonMapper.readValue(msg, MessageOperation.class);
            operation(accessToken, messageOperation);
        } catch (Exception e) {
            log.error("消息解析失败: {}", msg, e);
        }
    }

    /**
     * IM操作
     *
     * @param accessToken
     * @param messageOperation
     */
    private void operation(String accessToken, MessageOperation messageOperation) {

        AuthUser authUser = UserContext.getAuthUser(cache, accessToken);
        switch (messageOperation.getOperationType()) {
            case PING:
                break;
            case MESSAGE:
                //保存消息
                ImMessage imMessage = new ImMessage(messageOperation);
                imMessageService.save(imMessage);
                //修改最后消息信息
                imTalkService.updateLastTalkMessage(messageOperation.getTalkId(), messageOperation.getContext(),
                    imMessage.getCreateTime(), imMessage.getMessageType() == null ? null : imMessage.getMessageType().name());
                //发送消息
                sendMessage(messageOperation.getTo(), new MessageVO(MessageResultType.MESSAGE, imMessage));
                break;
            case READ:
                if (StrUtil.isNotEmpty(messageOperation.getContext())) {
                    imMessageService.read(messageOperation.getTalkId(), accessToken);
                }
                break;
            case UNREAD:
                sendMessage(authUser.getId(),
                    new MessageVO(MessageResultType.UN_READ, imMessageService.unReadMessages(accessToken)));
                break;
            case HISTORY:
                sendMessage(authUser.getId(), new MessageVO(MessageResultType.HISTORY,
                    imMessageService.historyMessage(accessToken, messageOperation.getTo())));
                break;
            default:
                break;
        }
    }

    /**
     * 发送消息
     *
     * @param sessionId sessionId
     * @param message   消息对象
     */
    private void sendMessage(String sessionId, MessageVO message) {
        Session session = sessionPools.get(sessionId);
        sendMessage(session, message);
    }

    /**
     * 发送消息
     *
     * @param session 会话
     * @param message 消息对象
     */
    private void sendMessage(Session session, MessageVO message) {
        if (session != null) {
            try {
                session.getBasicRemote().sendText(JSONUtil.toJsonStr(message));
            } catch (Exception e) {
                log.error("发送消息异常", e);
            }
        }
    }

    /**
     * socket exception
     *
     * @param session
     * @param throwable
     */
    @OnError
    public void onError(Session session, Throwable throwable) {
        log.error("socket异常: {}", session.getId(), throwable);
    }

}
