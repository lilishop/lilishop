package cn.lili.modules.goods.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.util.Date;

/**
 * 商家卡池列表项
 *
 * @author Mike
 * @date 2026-07-31
 */
@Data
@Schema(description = "卡池列表项")
public class CardKeyPoolVO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "卡密记录 ID")
    private String id;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "商品 ID")
    private String goodsId;

    @Schema(description = "卡号")
    private String cardNo;

    @Schema(description = "卡密明文")
    private String cardSecret;

    @Schema(description = "状态 UNUSED/ALLOCATED/VOIDED")
    private String status;

    @Schema(description = "订单号")
    private String orderSn;

    @Schema(description = "订单项号")
    private String orderItemSn;

    @Schema(description = "发卡时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date allocatedTime;

    @Schema(description = "导入时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;
}
