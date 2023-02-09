package cn.lili.modules.im.entity.vo;

import cn.lili.modules.im.entity.enums.MessageResultType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;

/**
 * MessageVO
 *
 * @author Chopper
 * @version v1.0
 * 2021-12-30 15:51
 */
@Data
@Builder
@AllArgsConstructor
public class MessageVO {

    /**
     * 消息类型
     */
    private MessageResultType messageResultType;
    /**
     * 消息内容
     */
    private Object result;
}
