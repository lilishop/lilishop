package cn.lili.controller.im;

import cn.lili.cache.Cache;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.im.config.CustomSpringConfigurator;
import cn.lili.modules.im.entity.dos.ImMessage;
import cn.lili.modules.im.entity.enums.MessageResultType;
import cn.lili.modules.im.entity.vo.MessageOperation;
import cn.lili.modules.im.entity.vo.MessageVO;
import cn.lili.modules.im.service.ImMessageService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.StoreService;
import com.alibaba.druid.util.StringUtils;
import com.alibaba.fastjson.JSON;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.websocket.*;
import javax.websocket.server.PathParam;
import javax.websocket.server.ServerEndpoint;
import java.io.IOException;
import java.util.Date;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author liushuai
 */
@Component
@ServerEndpoint(value = "/lili/webSocket/{accessToken}", configurator = CustomSpringConfigurator.class)
@Slf4j
public class WebSocketServer {
    /**
     * 消息服务
     */
    @Autowired
    private ImMessageService imMessageService;

    /**
     * im用户服务
     */
    @Autowired
    private MemberService memberService;

    @Autowired
    private StoreService storeService;


    @Autowired
    private Cache cache;
    /**
     * 在线人数
     * PS 注意，只能单节点，如果多节点部署需要自行寻找方案
     */
    private static ConcurrentHashMap<String, Session> sessionPools = new ConcurrentHashMap<>();

    /**
     * 建立连接
     *
     * @param session
     */
    @OnOpen
    public void onOpen(@PathParam("accessToken") String accessToken, Session session) throws IOException {
        AuthUser authUser = UserContext.getAuthUser(accessToken);
        Object message = null;
        if (UserEnums.STORE.equals(authUser.getRole())) {
            message = storeService.getById(authUser.getStoreId());
            sessionPools.put(authUser.getStoreId(), session);

        } else if (UserEnums.MEMBER.equals(authUser.getRole())) {
            message = memberService.getById(authUser.getId());
            sessionPools.put(authUser.getId(), session);
        }
        MessageVO messageVO = new MessageVO(MessageResultType.FRIENDS, message);
        sendMessage(authUser.getId(), messageVO);
    }

    /**
     * 关闭连接
     */
    @OnClose
    public void onClose(@PathParam("accessToken") String accessToken) {
        log.info("断开连接:{}", accessToken);
        sessionPools.remove(UserContext.getAuthUser(accessToken).getId());
    }

    /**
     * 发送消息
     *
     * @param msg
     * @throws IOException
     */
    @OnMessage
    public void onMessage(@PathParam("accessToken") String accessToken, String msg) {
        log.error(msg);
        MessageOperation messageOperation = JSON.parseObject(msg, MessageOperation.class);
        operation(accessToken, messageOperation);
    }

    /**
     * IM操作
     *
     * @param accessToken
     * @param messageOperation
     */
    private void operation(String accessToken, MessageOperation messageOperation) {

        AuthUser authUser = UserContext.getAuthUser(accessToken);
        switch (messageOperation.getOperationType()) {
            case PING:
                break;
            case MESSAGE:
                //保存消息
                ImMessage imMessage = new ImMessage();
                imMessage.setFromUser(messageOperation.getFrom());
                imMessage.setMessageType(messageOperation.getMessageType());
                imMessage.setIsRead(false);
                imMessage.setText(messageOperation.getContext());
                imMessage.setTalkId(messageOperation.getTalkId());
                imMessage.setCreateTime(new Date());
                imMessage.setToUser(messageOperation.getTo());
                imMessage.setId(SnowFlake.getIdStr());
                imMessageService.save(imMessage);
                //发送消息
                sendMessage(messageOperation.getTo(), new MessageVO(MessageResultType.MESSAGE, imMessage));
                break;
            case READ:
                if (!StringUtils.isEmpty(messageOperation.getContext())) {
                    imMessageService.read(messageOperation.getTalkId(), accessToken);
                }
                break;
            case UNREAD:
                sendMessage(authUser.getId(), new MessageVO(MessageResultType.UN_READ, imMessageService.unReadMessages(accessToken)));
                break;
            case HISTORY:
                sendMessage(authUser.getId(), new MessageVO(MessageResultType.HISTORY, imMessageService.historyMessage(accessToken, messageOperation.getTo())));
                break;
            default:
                break;
        }
    }

    /**
     * 发送消息
     *
     * @param key     密钥
     * @param message 消息对象
     */
    private void sendMessage(String key, MessageVO message) {
        Session session = sessionPools.get(key);
        if (session != null) {
            try {
                session.getBasicRemote().sendText(JSON.toJSONString(message, true));
            } catch (IOException e) {
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
        throwable.printStackTrace();
    }

    /**
     * 获取店铺id
     *
     * @return
     */
    private String storeKey(String storeId) {
        return "STORE_" + storeId;
    }

}
