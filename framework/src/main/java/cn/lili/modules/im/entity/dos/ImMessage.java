package cn.lili.modules.im.entity.dos;

import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.im.entity.enums.MessageTypeEnum;
import cn.lili.modules.im.entity.vo.MessageOperation;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * @author Chopper
 */
@Data
@TableName("li_im_message")
@ApiModel(value = "Im消息")
@NoArgsConstructor
@AllArgsConstructor
public class ImMessage extends BaseEntity {

    private static final long serialVersionUID = 1L;

    /**
     * 发送者
     */
    private String fromUser;

    /**
     * 接收者
     */
    private String toUser;

    /**
     * 已阅
     */
    private Boolean isRead;

    /**
     * 消息类型
     */
    private MessageTypeEnum messageType;

    /**
     * 聊天id
     */
    private String talkId;

    /**
     * 消息实体
     */
    private String text;


    public ImMessage(MessageOperation messageOperation){
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