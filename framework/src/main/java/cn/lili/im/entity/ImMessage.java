package cn.lili.im.entity;

import cn.lili.im.entity.enums.MessageType;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import lombok.Data;

/**
 * @author Chopper
 */
@Data
@TableName("li_im_message")
@ApiModel(value = "Im消息")
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
    private MessageType messageType;

    /**
     * 聊天id
     */
    private String talkId;

    /**
     * 消息实体
     */
    private String text;

}