package cn.lili.modules.message.util;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum;

/**
 * 消息节点文案解析器：将库中 notice_node 文案映射回消息节点枚举。
 * 独立成工具类而非枚举静态方法——hutool 反序列化枚举时会优先调用枚举上单参数静态工厂方法，
 * 放在枚举内会导致 JSON 反序列化枚举名失败（ConvertException）。
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
public class NoticeMessageNodeResolver {

    /**
     * 将库中 {@code notice_node} 文案解析为枚举名，供 {@code scene_code} 与微信模板关联。
     *
     * @param noticeNode 消息模板中的应用场景文案
     * @return 匹配的消息节点枚举；无匹配或入参为空时返回 null
     */
    public static NoticeMessageNodeEnum fromNoticeNodeLabel(String noticeNode) {
        if (CharSequenceUtil.isBlank(noticeNode)) {
            return null;
        }
        String t = noticeNode.replace("\r", "").replace("\n", "").trim();
        for (NoticeMessageNodeEnum e : NoticeMessageNodeEnum.values()) {
            if (e.getDescription().equals(t)) {
                return e;
            }
        }
        if ("余额提现申请驳回通知".equals(t) || "提现申请驳回通知".equals(t)) {
            return NoticeMessageNodeEnum.WALLET_WITHDRAWAL_AUDIT_ERROR;
        }
        return null;
    }
}
