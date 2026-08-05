package cn.lili.modules.distribution.entity.vos;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 分销员推广/邀请订单详情
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class DistributionBuyerOrderDetailVO extends DistributionBuyerOrderVO {

    @Schema(description = "订单类型：PROMOTION / INVITE")
    private String orderType;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "支付时间")
    private Date paymentTime;

    @Schema(description = "订单总价")
    private BigDecimal totalOrderPrice;

    @Schema(description = "商品总件数")
    private Integer totalGoodsNum;

    @Schema(description = "订单状态")
    private String orderStatus;

    @Schema(description = "付款状态")
    private String payStatus;

    @Schema(description = "发货状态")
    private String deliverStatus;

    @Schema(description = "配送方式")
    private String deliveryMethod;

    @Schema(description = "物流公司名称")
    private String logisticsName;

    @Schema(description = "物流单号")
    private String logisticsNo;

    @Schema(description = "佣金标签文案")
    private String commissionLabel;
}
