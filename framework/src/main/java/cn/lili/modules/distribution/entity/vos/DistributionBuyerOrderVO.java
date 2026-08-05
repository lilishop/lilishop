package cn.lili.modules.distribution.entity.vos;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 分销员推广/邀请订单列表项
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionBuyerOrderVO {

    @Schema(description = "订单编号")
    private String orderSn;

    @Schema(description = "子订单编号")
    private String orderItemSn;

    @Schema(description = "买家名称")
    private String memberName;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "下单时间")
    private Date createTime;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "商品图片")
    private String image;

    @Schema(description = "规格")
    private String specs;

    @Schema(description = "商品数量")
    private Integer num;

    @Schema(description = "实付金额")
    private BigDecimal finalPrice;

    @Schema(description = "佣金金额")
    private BigDecimal commissionAmount;

    @Schema(description = "结算状态类型", allowableValues = "SETTLED,PENDING,NOT_SETTLE")
    private String settlementStatusType;

    @Schema(description = "结算状态文案")
    private String settlementStatusText;

    @Schema(description = "是否已退款")
    private Boolean refunded;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "结算时间")
    private Date settleTime;
}
