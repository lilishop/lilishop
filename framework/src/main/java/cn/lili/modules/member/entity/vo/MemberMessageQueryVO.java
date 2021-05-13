package cn.lili.modules.member.entity.vo;

import cn.lili.modules.message.entity.enums.MessageStatusEnum;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 会员消息查询
 *
 * @author Chopper
 * @date 2020-02-25 14:10:16
 */
@Data
public class MemberMessageQueryVO {

    private static final long serialVersionUID = 1L;

    /**
     * @see MessageStatusEnum
     */
    @ApiModelProperty(value = "状态")
    private String status;

    @ApiModelProperty(value = "消息标题")
    private String title;

    @ApiModelProperty(value = "会员id")
    private String memberId;

}