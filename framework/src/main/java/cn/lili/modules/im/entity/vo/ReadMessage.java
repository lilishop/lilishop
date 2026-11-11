package cn.lili.modules.im.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * 已读消息VO
 *
 * @author Chopper
 * @version v1.0
 * 2021-12-31 11:13
 */
@Data
@Schema(description = "已读消息VO")
@NoArgsConstructor
@AllArgsConstructor
public class ReadMessage {

    @Schema(description = "已读消息ID列表")
    private List<String> readMessageList;
}
