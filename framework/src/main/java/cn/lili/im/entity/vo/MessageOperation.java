package cn.lili.im.entity.vo;

import cn.lili.common.utils.StringUtils;
import cn.lili.im.entity.enums.MessageType;
import cn.lili.im.entity.enums.OperationType;
import lombok.Data;

/**
 * @author liushuai
 */
@Data
public class MessageOperation {

    /**
     * 消息类型
     */
    private OperationType operationType;
    /**
     * 与某人聊天记录
     */
    private String to;

    /**
     * 聊天id
     */
    private String talkId;

    /**
     * 消息类型
     */
    private MessageType messageType;
    /**
     * 消息内容
     */
    private String context;

    public void setOperationType(String operationType) {
        if (!StringUtils.isEmpty(operationType)) {
            this.operationType = OperationType.valueOf(operationType);
        }
    }

    public void setMessageType(String messageType) {
        if (!StringUtils.isEmpty(messageType)) {
            this.messageType = MessageType.valueOf(messageType);
        }
    }
}
