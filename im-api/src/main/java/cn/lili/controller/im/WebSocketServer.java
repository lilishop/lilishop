package cn.lili.controller.im;

import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.im.config.CustomSpringConfigurator;
import cn.lili.modules.im.entity.dos.ImMessage;
import cn.lili.modules.im.entity.dos.ImTalk;
import cn.lili.modules.im.entity.enums.MessageResultType;
import cn.lili.modules.im.entity.vo.MessageOperation;
import cn.lili.modules.im.entity.vo.MessageVO;
import cn.lili.modules.im.service.ImMessageService;
import cn.lili.modules.im.service.ImTalkService;
import com.alibaba.druid.util.StringUtils;
import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import java.io.IOException;
import java.util.concurrent.ConcurrentHashMap;
import javax.websocket.OnClose;
import javax.websocket.OnError;
import javax.websocket.OnMessage;
import javax.websocket.OnOpen;
import javax.websocket.Session;
import javax.websocket.server.PathParam;
import javax.websocket.server.ServerEndpoint;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

/**
 * 前端伪装术（Sec-WebSocket-Protocol）
 * 让前端绝对不要再把 Token 拼在 URL 后面裸奔了！
 * 标准的前端 WebSocket API 虽然不能自定义 Header，但它支持**“子协议（SubProtocol）”**！
 * 前端这么写：
 * JavaScript
 * // 前端大神的骚操作：把 Token 伪装成子协议传过来！
 * const token = "eyJhbGciOiJIUzI1Ni...真实Token...";
 * const ws = new WebSocket("ws://localhost:8080/lili/webSocket", [token]);
 * 这样，Token 就会被包装在 HTTP 请求头的 Sec-WebSocket-Protocol 字段里，安全且符合标准！
 */
@Component
@ServerEndpoint(value = "/lili/webSocket", configurator = CustomSpringConfigurator.class)
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
                e.printStackTrace();
            }
        }
        sessionPools.put(sessionId, session);
    }

    /**
     * 关闭连接
     */
    @OnClose
    public void onClose() {
        AuthUser authUser = UserContext.getAuthUser();
        log.info("用户断开断开连接:{}", JSONUtil.toJsonStr(authUser));
        sessionPools.remove(authUser);
    }

    /**
     * 发送消息
     *
     * @param message
     * @throws IOException
     */
    @OnMessage
    public void onMessage(String message, Session session) {
        // 1. 从当前 WebSocket 会话中取出握手时存入的用户信息
        AuthUser user = (AuthUser) session.getUserProperties().get("CURRENT_USER");
        log.info("发送消息：{}", message);
        try {
            // 2. 👑 移花接木：把用户信息强行注入当前线程的 UserContext！
            UserContext.set(user);

            MessageOperation messageOperation = JSON.parseObject(message, MessageOperation.class);
            operation(messageOperation);
            // 3. 愉快地调用你的 Service 吧！
            // 此时无论底层的 Service 怎么调用 UserContext.getCurrentUser()，都能完美拿到！
            // memberService.doSomething();
            log.info("用户 [{}] 发来了消息: {}", UserContext.getCurrentUserId(), message);

        } finally {
            // 4. 🧹 架构师的底线：不管发生什么，执行完必须清空当前线程的上下文！
            UserContext.clear();
        }
    }

    /**
     * IM操作
     *
     * @param messageOperation
     */
    private void operation(MessageOperation messageOperation) {

        AuthUser authUser = UserContext.getAuthUser();
        switch (messageOperation.getOperationType()) {
            case PING:
                break;
            case MESSAGE:
                //保存消息
                ImMessage imMessage = new ImMessage(messageOperation);
                imMessageService.save(imMessage);
                //修改最后消息信息
                imTalkService.update(new LambdaUpdateWrapper<ImTalk>().eq(ImTalk::getId, messageOperation.getTalkId())
                    .set(ImTalk::getLastTalkMessage, messageOperation.getContext())
                    .set(ImTalk::getLastTalkTime, imMessage.getCreateTime())
                    .set(ImTalk::getLastMessageType, imMessage.getMessageType()));
                //发送消息
                sendMessage(messageOperation.getTo(), new MessageVO(MessageResultType.MESSAGE, imMessage));
                break;
            case READ:
                if (!StringUtils.isEmpty(messageOperation.getContext())) {
                    imMessageService.read(messageOperation.getTalkId());
                }
                break;
            case UNREAD:
                sendMessage(authUser.getId(),
                    new MessageVO(MessageResultType.UN_READ, imMessageService.unReadMessages()));
                break;
            case HISTORY:
                sendMessage(authUser.getId(), new MessageVO(MessageResultType.HISTORY,
                    imMessageService.historyMessage(messageOperation.getTo())));
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
                session.getBasicRemote().sendText(JSON.toJSONString(message, true));
            } catch (Exception e) {
                e.printStackTrace();
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
