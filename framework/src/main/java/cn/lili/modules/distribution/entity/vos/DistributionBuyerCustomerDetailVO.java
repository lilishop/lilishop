package cn.lili.modules.distribution.entity.vos;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 分销员端客户详情
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionBuyerCustomerDetailVO {

    @Schema(description = "关系ID")
    private String id;

    @Schema(description = "客户会员ID")
    private String memberId;

    @Schema(description = "客户昵称")
    private String memberNickname;

    @Schema(description = "客户头像")
    private String memberAvatar;

    @Schema(description = "客户手机号")
    private String memberMobile;

    @Schema(description = "是否星标")
    private Boolean starred;

    @Schema(description = "订单数量")
    private Long orderCount = 0L;

    @Schema(description = "成交金额")
    private BigDecimal tradeAmount = BigDecimal.ZERO;

    @Schema(description = "客单价")
    private BigDecimal averageOrderAmount = BigDecimal.ZERO;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "最后成交时间")
    private Date lastTradeTime;
}
