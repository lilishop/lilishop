package cn.lili.modules.im.entity.vo;

import cn.lili.modules.im.entity.enums.MessageResultType;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 消息返回VO
 *
 * @author Chopper
 * @version v1.0
 * 2021-12-30 15:51
 */
@Data
@Builder
@Schema(description = "消息返回VO")
@NoArgsConstructor
@AllArgsConstructor
public class MessageVO {

    @Schema(description = "返回消息类型")
    private MessageResultType messageResultType;

    @Schema(description = "消息内容")
    private Object result;
}
