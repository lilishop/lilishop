package cn.lili.modules.wechat.support;

import cn.lili.common.exception.ServiceException;
import lombok.Getter;

/**
 * 携带可审计业务码的上报异常，不携带请求正文或凭据。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
@Getter
public class WechatOrderSyncException extends ServiceException {
    /** 业务码或微信错误码。 */
    private final String code;
    /** true 表示尚待履约或开关条件满足；false 表示需要纠正的错误。 */
    private final boolean pending;

    /** 构造上报校验或接口拒绝结果。 */
    public WechatOrderSyncException(String code, String message, boolean pending) {
        super(message);
        this.code = code;
        this.pending = pending;
    }
}
