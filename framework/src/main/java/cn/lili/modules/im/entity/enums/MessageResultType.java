package cn.lili.modules.im.entity.enums;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * 返回消息类型枚举
 *
 * @author liushuai
 */
@Schema(description = "返回消息类型")
public enum MessageResultType {

    FRIENDS,
    ADD_FRIENDS,
    MESSAGE,
    READ_MESSAGE,
    UN_READ,
    HISTORY,
    SYSTEM_TIPS,
    OFFLINE

}
