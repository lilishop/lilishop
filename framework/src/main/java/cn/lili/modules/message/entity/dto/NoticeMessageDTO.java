package cn.lili.modules.message.entity.dto;

import cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.Map;

/**
 * 站内信消息
 *
 * @author Chopper
 * @since 2020/12/8 9:46
 */
@Data
public class NoticeMessageDTO {

    @Schema(description = "客户ID")
    private String memberId;

    @Schema(description = "消息节点")
    private NoticeMessageNodeEnum noticeMessageNodeEnum;

    @Schema(description = "消息参数")
    private Map<String, String> parameter;

    @Schema(description = "微信小程序订阅消息跳转路径")
    private String page;
}
