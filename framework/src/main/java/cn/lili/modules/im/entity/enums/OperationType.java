package cn.lili.modules.im.entity.enums;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * 操作类型枚举
 *
 * @author liushuai
 */
@Schema(description = "操作类型")
public enum OperationType {

    PING,
    CREATE,
    MESSAGE,
    HISTORY,
    READ,
    UNREAD

}
