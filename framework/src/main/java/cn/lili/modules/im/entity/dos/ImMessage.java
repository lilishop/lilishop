package cn.lili.modules.im.entity.dos;

import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.im.entity.enums.MessageTypeEnum;
import cn.lili.modules.im.entity.vo.MessageOperation;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * IM消息
 *
 * @author Chopper
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("li_im_message")
@Schema(description = "IM消息")
@NoArgsConstructor
@AllArgsConstructor
public class ImMessage extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "发送者")
    private String fromUser;

    @Schema(description = "接收者")
    private String toUser;

    @Schema(description = "已读状态")
    private Boolean isRead;

    @Schema(description = "消息类型")
    private MessageTypeEnum messageType;

    @Schema(description = "聊天会话ID")
    private String talkId;

    @Schema(description = "消息内容")
    private String text;

    public ImMessage(MessageOperation messageOperation) {
        this.setFromUser(messageOperation.getFrom());
        this.setMessageType(messageOperation.getMessageType());
        this.setIsRead(false);
        this.setText(messageOperation.getContext());
        this.setTalkId(messageOperation.getTalkId());
        this.setCreateTime(new Date());
        this.setToUser(messageOperation.getTo());
        this.setId(SnowFlake.getIdStr());
    }

}
