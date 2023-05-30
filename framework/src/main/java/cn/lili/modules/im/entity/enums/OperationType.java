package cn.lili.modules.im.entity.enums;

/**
 * 操作类型枚举
 *
 * @author liushuai
 */
public enum OperationType {
    /**
     * 消息类型枚举
     * <p>
     * 心跳检测
     * 发起聊天
     * 发起消息
     * 查询历史消息
     * 阅读消息
     * 查询未读消息
     */
    PING,
    CREATE,
    MESSAGE,
    HISTORY,
    READ,
    UNREAD,

}
