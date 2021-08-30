package cn.lili.modules.message.entity.dos;

import cn.lili.mybatis.BaseEntity;
import cn.lili.modules.message.entity.enums.MessageStatusEnum;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 会员接受消息对象
 *
 * @author Chopper
 * @since 2020-02-25 14:10:16
 */
@Data
@TableName("li_member_message")
@ApiModel(value = "会员消息")
public class MemberMessage extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "会员id")
    private String memberId;

    @ApiModelProperty(value = "会员名称")
    private String memberName;

    @ApiModelProperty(value = "消息标题")
    private String title;

    @ApiModelProperty(value = "消息内容")
    private String content;

    @ApiModelProperty(value = "关联消息id")
    private String messageId;

    /**
     * @see MessageStatusEnum
     */
    @ApiModelProperty(value = "状态")
    private String status = MessageStatusEnum.UN_READY.name();

}