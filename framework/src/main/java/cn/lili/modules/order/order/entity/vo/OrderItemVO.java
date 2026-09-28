package cn.lili.modules.order.order.entity.vo;

import cn.lili.modules.order.order.entity.enums.CommentStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 子订单VO
 *
 * @author Chopper
 * @since 2020-08-17 20:28
 */
@Data
@NoArgsConstructor
public class OrderItemVO {

    @Schema(description = "编号")
    private String sn;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "货品ID")
    private String skuId;

    @Schema(description = "销售量")
    private String num;

    @Schema(description = "图片")
    private String image;

    @Schema(description = "商品名称")
    private String name;

    @Schema(description = "规格")
    private String specs;

    @Schema(description = "商品名称")
    private Double goodsPrice;

    /**
     * @see OrderItemAfterSaleStatusEnum
     */
    @Schema(description = "售后状态", allowableValues = "NOT_APPLIED(未申请),ALREADY_APPLIED(已申请),EXPIRED(已失效不允许申请售后)")
    private String afterSaleStatus;

    /**
     * @see OrderComplaintStatusEnum
     */
    @Schema(description = "投诉状态")
    private String complainStatus;

    /**
     * @see CommentStatusEnum
     */
    @Schema(description = "评论状态:未评论(UNFINISHED),待追评(WAIT_CHASE),评论完成(FINISHED)，")
    private String commentStatus;

    /**
     * @see cn.lili.modules.order.order.entity.enums.RefundStatusEnum
     */
    @Schema(description = "退款状态")
    private String isRefund;

    @Schema(description = "退款金额")
    private String refundPrice;

    public void setSn(String sn) {
        this.sn = sn;
    }

    public void setSkuId(String skuId) {
        this.skuId = skuId;
    }

    public void setNum(String num) {
        this.num = num;
    }

    public void setImage(String image) {
        this.image = image;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setGoodsPrice(Double goodsPrice) {
        this.goodsPrice = goodsPrice;
    }

    public void setAfterSaleStatus(String afterSaleStatus) {
        this.afterSaleStatus = afterSaleStatus;
    }

    public void setComplainStatus(String complainStatus) {
        this.complainStatus = complainStatus;
    }

    public void setCommentStatus(String commentStatus) {
        this.commentStatus = commentStatus;
    }
}
