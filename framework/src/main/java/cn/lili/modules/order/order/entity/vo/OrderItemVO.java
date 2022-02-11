package cn.lili.modules.order.order.entity.vo;

import cn.lili.modules.order.order.entity.enums.CommentStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 子订单VO
 *
 * @author Chopper
 * @since 2020-08-17 20:28
 */
@Data
public class OrderItemVO {

    @ApiModelProperty(value = "编号")
    private String sn;

    @ApiModelProperty(value = "商品ID")
    private String goodsId;

    @ApiModelProperty(value = "货品ID")
    private String skuId;

    @ApiModelProperty(value = "销售量")
    private String num;

    @ApiModelProperty(value = "图片")
    private String image;

    @ApiModelProperty(value = "商品名称")
    private String name;

    @ApiModelProperty(value = "商品名称")
    private Double goodsPrice;

    /**
     * @see OrderItemAfterSaleStatusEnum
     */
    @ApiModelProperty(value = "售后状态", allowableValues = "NOT_APPLIED(未申请),ALREADY_APPLIED(已申请),EXPIRED(已失效不允许申请售后)")
    private String afterSaleStatus;

    /**
     * @see OrderComplaintStatusEnum
     */
    @ApiModelProperty(value = "投诉状态")
    private String complainStatus;

    /**
     * @see CommentStatusEnum
     */
    @ApiModelProperty(value = "评论状态:未评论(UNFINISHED),待追评(WAIT_CHASE),评论完成(FINISHED)，")
    private String commentStatus;


    public OrderItemVO(String sn, String goodsId, String skuId, String num, String image, String name, String afterSaleStatus, String complainStatus, String commentStatus, Double goodsPrice) {
        this.sn = sn;
        this.goodsId = goodsId;
        this.skuId = skuId;
        this.num = num;
        this.image = image;
        this.name = name;
        this.afterSaleStatus = afterSaleStatus;
        this.complainStatus = complainStatus;
        this.commentStatus = commentStatus;
        this.goodsPrice = goodsPrice;
    }

}
