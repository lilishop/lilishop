package cn.lili.modules.wechat.support;

import cn.lili.common.exception.ServiceException;
import cn.lili.modules.wechat.entity.dto.WechatOrderSearchParams;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * 验证专用列表服务端范围、参数化状态筛选及无记录历史订单的待核查口径。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
class WechatOrderSearchParamsTest {
    /** 地址栏或请求篡改来源与支付方式仍不能扩大专用列表范围。 */
    @Test
    void forcesMiniProgramWechatPaymentScope() {
        WechatOrderSearchParams params = new WechatOrderSearchParams();
        params.setClientType("PC"); params.setPaymentMethod("WALLET");
        QueryWrapper<?> query = params.queryWrapper(); query.getSqlSegment();
        assertEquals("WECHAT_MP", params.getClientType());
        assertEquals("WECHAT", params.getPaymentMethod());
        assertTrue(query.getParamNameValuePairs().containsValue("WECHAT_MP"));
        assertTrue(query.getParamNameValuePairs().containsValue("WECHAT"));
        assertFalse(query.getParamNameValuePairs().containsValue("PC"));
    }

    /** 无记录查询必须使用 NOT EXISTS，不能猜测历史上报成功。 */
    @Test
    void uncheckedMeansNoLocalRecord() {
        WechatOrderSearchParams params = new WechatOrderSearchParams(); params.setWechatSyncStatus("UNCHECKED");
        String sql = params.queryWrapper().getSqlSegment();
        assertTrue(sql.contains("NOT EXISTS"));
        assertTrue(sql.contains("ORDER:"));
        assertTrue(sql.contains("o.receivable_no"));
    }

    /** 状态通过绑定参数进入查询，不拼接用户输入。 */
    @Test
    void failedStatusUsesBoundParameter() {
        WechatOrderSearchParams params = new WechatOrderSearchParams(); params.setWechatSyncStatus("FAILED");
        QueryWrapper<?> query = params.queryWrapper(); String sql = query.getSqlSegment();
        assertTrue(sql.contains("EXISTS"));
        assertFalse(sql.contains("'FAILED'"));
        assertTrue(query.getParamNameValuePairs().containsValue("FAILED"));
    }

    /** 非法状态在业务层拒绝，避免将未知字符串直接用于查询。 */
    @Test
    void rejectsUnknownStatus() {
        WechatOrderSearchParams params = new WechatOrderSearchParams(); params.setWechatSyncStatus("FAILED' OR 1=1");
        assertThrows(ServiceException.class, params::queryWrapper);
    }
}
