package cn.lili.modules.payment.kit.params.dto;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.utils.StringUtils;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.ToString;

import java.util.Date;
import java.util.List;

/**
 * 支付参数
 *
 * @author Chopper
 * @since 2021-01-25 19:09
 */
@Data
@ToString
public class CashierParam {

    static final Long MAX_DETAIL_LENGTH = 30L;

    @Schema(description = "价格")
    private Double price;

    @Schema(description = "支付title")
    private String title;

    @Schema(description = "支付详细描述")
    private String detail;

    @Schema(description = "订单sn集合")
    private String orderSns;

    @Schema(description = "支持支付方式")
    private List<String> support;


    @Schema(description = "订单创建时间")
    private Date createTime;

    @Schema(description = "支付自动结束时间")
    private Long autoCancel;

    @Schema(description = "剩余余额")
    private Double walletValue;

    @Schema(description = "订单促销类型(NORMAL/PINTUAN等)")
    private String orderPromotionType;

    public String getDetail() {
        if (CharSequenceUtil.isEmpty(detail)) {
            return "清单详细";
        }
        return StringUtils.filterSpecialChart(StringUtils.sub(detail, 30));
    }
}
