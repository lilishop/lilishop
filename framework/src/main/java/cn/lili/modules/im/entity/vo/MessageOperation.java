package cn.lili.modules.im.entity.vo;

import cn.lili.common.utils.StringUtils;
import cn.lili.modules.im.entity.enums.MessageTypeEnum;
import cn.lili.modules.im.entity.enums.OperationType;
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
     * 发送者
     */
    private String from;

    /**
     * 聊天id
     */
    private String talkId;

    /**
     * 消息类型
     */
    private MessageTypeEnum messageType;
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
            this.messageType = MessageTypeEnum.valueOf(messageType);
        }
    }
}
