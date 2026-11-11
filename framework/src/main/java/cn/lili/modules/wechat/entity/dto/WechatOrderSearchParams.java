package cn.lili.modules.wechat.entity.dto;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.wechat.entity.enums.WechatOrderSyncStatus;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 小程序订单专用查询；服务端固定来源和微信支付范围，并支持同步状态筛选。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class WechatOrderSearchParams extends OrderSearchParams {
    /** 发货同步状态，取值参见 WechatOrderSyncStatus；空值查询全部。 */
    private String wechatSyncStatus;

    /** 复用订单筛选条件，状态值参数化且强制限定小程序微信支付订单。 */
    @Override
    public <T> QueryWrapper<T> queryWrapper() {
        setClientType("WECHAT_MP");
        setPaymentMethod("WECHAT");
        QueryWrapper<T> wrapper = super.queryWrapper();
        wrapper.eq("o.client_type", "WECHAT_MP");
        if (CharSequenceUtil.isNotBlank(wechatSyncStatus)) {
            try {
                WechatOrderSyncStatus.valueOf(wechatSyncStatus);
            } catch (IllegalArgumentException e) {
                throw new ServiceException("无效的微信上报状态");
            }
            String relation = "SELECT 1 FROM li_wechat_order_sync ws WHERE ws.transaction_id = "
                    + "CASE WHEN o.receivable_no IS NULL OR TRIM(o.receivable_no) = '' THEN CONCAT('ORDER:', o.sn) ELSE o.receivable_no END";
            if ("UNCHECKED".equals(wechatSyncStatus)) {
                wrapper.apply("NOT EXISTS (" + relation + ")");
            } else {
                wrapper.apply("EXISTS (" + relation + " AND ws.status = {0})", wechatSyncStatus);
            }
        }
        return wrapper;
    }
}
