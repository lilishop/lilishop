package cn.lili.modules.order.order.entity.dos;

import cn.hutool.core.collection.CollUtil;
import com.alibaba.fastjson2.JSON;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.goods.entity.vo.CardKeyVO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.CommentStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.RefundStatusEnum;
import cn.lili.modules.promotion.entity.vos.PromotionSkuVO;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.stream.Collectors;
import java.util.List;

/**
 * 子订单
 *
 * @author Chopper
 * @since 2020/11/17 7:30 下午
 */
@Data
@TableName("li_order_item")
@Schema(description = "子订单")
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class OrderItem extends BaseEntity {

    private static final long serialVersionUID = 2108971190191410182L;

    @Schema(description = "订单编号")
    private String orderSn;

    @Schema(description = "子订单编号")
    private String sn;

    @Schema(description = "单价")
    private Double unitPrice;

    @Schema(description = "小记")
    private Double subTotal;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "货品ID")
    private String skuId;

    @Schema(description = "销售量")
    private Integer num;

    @Schema(description = "交易编号")
    private String tradeSn;

    @Schema(description = "图片")
    private String image;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "分类ID")
    private String categoryId;

    @Schema(description = "快照id")
    private String snapshotId;

    @Schema(description = "规格json")
    private String specs;

    @Schema(description = "促销类型")
    private String promotionType;

    @Schema(description = "促销id")
    private String promotionId;

    @Schema(description = "销售金额")
    private Double goodsPrice;

    @Schema(description = "实际金额")
    private Double flowPrice;

    /**
     * @see CommentStatusEnum
     */
    @Schema(description = "评论状态:未评论(UNFINISHED),待追评(WAIT_CHASE),评论完成(FINISHED)，")
    private String commentStatus;

    /**
     * @see OrderItemAfterSaleStatusEnum
     */
    @Schema(description = "售后状态")
    private String afterSaleStatus;

    @Schema(description = "价格详情")
    private String priceDetail;

    /**
     * @see OrderComplaintStatusEnum
     */
    @Schema(description = "投诉状态")
    private String complainStatus;

    @Schema(description = "交易投诉id")
    private String complainId;

    @Schema(description = "退货商品数量")
    private Integer returnGoodsNumber;

    /**
     * @see cn.lili.modules.order.order.entity.enums.RefundStatusEnum
     */
    @Schema(description = "退款状态")
    private String isRefund;

    @Schema(description = "退款金额")
    private Double refundPrice;

    @Schema(description = "已发货数量")
    private Integer deliverNumber;

    /**
     * 订单详情扩展（不落库）：是否已发卡。
     * 仅 {@link cn.lili.modules.goods.service.CardKeyService#enrichOrderDetail} 对 E_COUPON 子单填充；
     * 非 E_COUPON 或未 enrich 时为 null；PENDING/FAILED 为 false；与
     * {@link #cardKeyFulfillStatus}=DELIVERED 且 {@link #cardKeys} 非空一致。
     */
    @TableField(exist = false)
    @Schema(description = "是否已发卡")
    private Boolean cardKeyDelivered;

    /**
     * 订单详情扩展（不落库）：明文卡号/卡密。
     * DELIVERED 时为非空列表；PENDING/FAILED 为空列表；非 E_COUPON 或未 enrich 时为 null。
     */
    @TableField(exist = false)
    @Schema(description = "卡密列表")
    private List<CardKeyVO> cardKeys;

    /**
     * 订单详情扩展（不落库）：卡密履约状态。
     *
     * @see cn.lili.modules.goods.entity.enums.CardKeyFulfillStatusEnum
     */
    @TableField(exist = false)
    @Schema(description = "卡密履约状态 PENDING/DELIVERED/FAILED/NOT_APPLICABLE")
    private String cardKeyFulfillStatus;

    /** 订单详情扩展（不落库）：卡密履约说明；DELIVERED 时通常为 null */
    @TableField(exist = false)
    @Schema(description = "卡密履约说明")
    private String cardKeyFulfillMessage;

    public Integer getDeliverNumber() {
        if(deliverNumber == null){
            return 0;
        }
        return deliverNumber;
    }

    public OrderItem(CartSkuVO cartSkuVO, CartVO cartVO, TradeDTO tradeDTO) {
        String oldId = this.getId();
        BeanUtil.copyProperties(cartSkuVO.getGoodsSku(), this);
        BeanUtil.copyProperties(cartSkuVO.getPriceDetailDTO(), this);
        BeanUtil.copyProperties(cartSkuVO, this);
        this.setId(oldId);
        if (cartSkuVO.getPriceDetailDTO().getJoinPromotion() != null && !cartSkuVO.getPriceDetailDTO().getJoinPromotion().isEmpty()) {
            this.setPromotionType(CollUtil.join(cartSkuVO.getPriceDetailDTO().getJoinPromotion().stream().map(PromotionSkuVO::getPromotionType).collect(Collectors.toList()), ","));
            this.setPromotionId(CollUtil.join(cartSkuVO.getPriceDetailDTO().getJoinPromotion().stream().map(PromotionSkuVO::getActivityId).collect(Collectors.toList()), ","));
        }
        this.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.NEW.name());
        this.setCommentStatus(CommentStatusEnum.NEW.name());
        this.setComplainStatus(OrderComplaintStatusEnum.NEW.name());
        this.setPriceDetailDTO(cartSkuVO.getPriceDetailDTO());
        this.setOrderSn(cartVO.getSn());
        this.setTradeSn(tradeDTO.getSn());
        this.setImage(cartSkuVO.getGoodsSku().getThumbnail());
        this.setGoodsName(cartSkuVO.getGoodsSku().getGoodsName());
        this.setSkuId(cartSkuVO.getGoodsSku().getId());
        this.setCategoryId(cartSkuVO.getGoodsSku().getCategoryPath().substring(
                cartSkuVO.getGoodsSku().getCategoryPath().lastIndexOf(",") + 1
        ));
        this.setGoodsPrice(cartSkuVO.getGoodsSku().getPrice());
        this.setUnitPrice(cartSkuVO.getPurchasePrice());
        this.setSubTotal(cartSkuVO.getSubTotal());
        this.setSn(SnowFlake.createStr("OI"));


    }

    public String getIsRefund() {
        if (isRefund == null) {
            return RefundStatusEnum.NO_REFUND.name();
        }
        return isRefund;
    }

    public double getRefundPrice() {
        if (refundPrice == null) {
            return 0;
        }
        return refundPrice;
    }

    public PriceDetailDTO getPriceDetailDTO() {
        return JSON.parseObject(priceDetail, PriceDetailDTO.class);
    }

    public void setPriceDetailDTO(PriceDetailDTO priceDetail) {
        this.priceDetail = JSON.toJSONString(priceDetail);
    }

    public String getAfterSaleStatus() {
        if (!PromotionTypeEnum.isCanAfterSale(this.promotionType)) {
            return OrderItemAfterSaleStatusEnum.EXPIRED.name();
        }
        return afterSaleStatus;
    }
}