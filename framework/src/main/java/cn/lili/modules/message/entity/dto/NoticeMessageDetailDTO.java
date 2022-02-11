package cn.lili.modules.message.entity.dto;

import cn.lili.modules.message.entity.dos.NoticeMessage;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * 站内信消息DTO
 * @author Chopper
 * @since 2020/12/8 9:46
 */
@Data
public class NoticeMessageDetailDTO extends NoticeMessage {

    @ApiModelProperty(value = "消息变量")
    private List<String> variables;
}
