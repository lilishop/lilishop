package cn.lili.modules.order.order.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 核销记录
 *
 * @author Bulbasaur
 * @since 2026-09-23
 */
@Data
@Schema(description = "核销记录")
public class OrderVerificationRecordVO {

    @Schema(description = "核销码")
    private String code;

    @Schema(description = "订单编号")
    private String orderSn;

    @Schema(description = "订单货物编号")
    private String orderItemSn;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "商品图片")
    private String image;

    @Schema(description = "规格")
    private String specs;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "核销时间")
    private Date verifiedTime;

    @Schema(description = "核销员")
    private String verifierName;
}
