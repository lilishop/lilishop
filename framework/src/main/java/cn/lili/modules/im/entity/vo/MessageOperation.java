package cn.lili.modules.im.entity.vo;

import cn.lili.common.utils.StringUtils;
import cn.lili.modules.im.entity.enums.MessageTypeEnum;
import cn.lili.modules.im.entity.enums.OperationType;
import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import tools.jackson.databind.annotation.JsonDeserialize;

/**
 * 消息操作VO
 *
 * @author liushuai
 */
@Data
@Schema(description = "消息操作VO")
@NoArgsConstructor
@AllArgsConstructor
public class MessageOperation {

    @Schema(description = "操作类型")
    @JsonProperty("operation_type")
    @JsonAlias({"operationType", "operation_type"})
    private OperationType operationType;

    @Schema(description = "接收方ID")
    private String to;

    @Schema(description = "发送方ID")
    private String from;

    @Schema(description = "聊天会话ID")
    @JsonProperty("talk_id")
    @JsonAlias({"talkId", "talk_id"})
    private String talkId;

    @Schema(description = "消息类型")
    @JsonProperty("message_type")
    @JsonAlias({"messageType", "message_type"})
    private MessageTypeEnum messageType;

    @Schema(description = "消息内容")
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
