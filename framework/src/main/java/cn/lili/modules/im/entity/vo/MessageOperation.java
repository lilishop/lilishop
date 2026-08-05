package cn.lili.modules.im.entity.vo;

import cn.lili.common.utils.StringUtils;
import cn.lili.modules.im.entity.enums.MessageTypeEnum;
import cn.lili.modules.im.entity.enums.OperationType;
import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import tools.jackson.databind.annotation.JsonDeserialize;

/**
 * @author liushuai
 */
@Data
public class MessageOperation {

    /**
     * 操作类型
     */
    @JsonProperty("operation_type")
    @JsonAlias({"operationType", "operation_type"})
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
    @JsonProperty("talk_id")
    @JsonAlias({"talkId", "talk_id"})
    private String talkId;

    /**
     * 消息类型
     */
    @JsonProperty("message_type")
    @JsonAlias({"messageType", "message_type"})
    private MessageTypeEnum messageType;
    /**
     * 消息内容
     */
    @JsonDeserialize(using = FlexibleJsonStringDeserializer.class)
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
