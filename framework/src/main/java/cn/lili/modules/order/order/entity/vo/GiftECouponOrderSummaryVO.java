package cn.lili.modules.order.order.entity.vo;

import cn.lili.modules.goods.entity.vo.CardKeyVO;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 满赠电子卡券赠品子单摘要（挂载于主单 {@link OrderDetailVO}）。
 *
 * @author Mike
 * @date 2026-08-02
 */
@Data
@Schema(description = "满赠电子卡券赠品子单摘要")
public class GiftECouponOrderSummaryVO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "赠品子单编号")
    private String orderSn;

    @Schema(description = "主单编号")
    private String parentOrderSn;

    @Schema(description = "子单状态")
    private String orderStatus;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "卡密履约状态 PENDING/DELIVERED/FAILED")
    private String cardKeyFulfillStatus;

    @Schema(description = "履约说明（失败原因或待发放提示）")
    private String cardKeyFulfillMessage;

    @Schema(description = "是否已发卡（与 cardKeys 非空一致）")
    private Boolean cardKeyDelivered;

    @Schema(description = "已发放卡密（仅 DELIVERED 时有值）")
    private List<CardKeyVO> cardKeys;
}
