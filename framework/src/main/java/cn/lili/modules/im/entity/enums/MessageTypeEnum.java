package cn.lili.modules.im.entity.enums;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * 消息类型
 *
 * @author liushuai
 */
@Schema(description = "消息类型")
public enum MessageTypeEnum {
    MESSAGE,
    PICTURE,
    VOICE,
    GOODS,
    ORDER,
    VIDEO
}
