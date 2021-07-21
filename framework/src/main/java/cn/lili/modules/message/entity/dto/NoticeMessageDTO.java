package cn.lili.modules.message.entity.dto;

import cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Map;

/**
 * 站内信消息
 * @author Chopper
 * @since 2020/12/8 9:46
 */
@Data
public class NoticeMessageDTO {

    @ApiModelProperty(value = "会员ID")
    private String memberId;

    @ApiModelProperty(value = "消息节点")
    private NoticeMessageNodeEnum noticeMessageNodeEnum;

    @ApiModelProperty(value = "消息参数")
    private Map<String,String> parameter;
}
