package cn.lili.modules.order.order.entity.vo;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.security.sensitive.Sensitive;
import cn.lili.common.security.sensitive.enums.SensitiveStrategy;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.Setter;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 订单简略信息
 * 用于订单列表查看
 *
 * @author Chopper
 * @since 2020-08-17 20:28
 */
@Data
public class OrderSimpleVO {

    @ApiModelProperty("sn")
    private String sn;

    @ApiModelProperty(value = "总价格")
    private Double flowPrice;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "创建时间")
    private Date createTime;

    /**
     * @see cn.lili.modules.order.order.entity.enums.OrderStatusEnum
     */
    @ApiModelProperty(value = "订单状态")
    private String orderStatus;

    /**
     * @see cn.lili.modules.order.order.entity.enums.PayStatusEnum
     */
    @ApiModelProperty(value = "付款状态")
    private String payStatus;

    @ApiModelProperty(value = "支付方式")
    private String paymentMethod;

    @ApiModelProperty(value = "支付时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date paymentTime;

    @ApiModelProperty(value = "用户名")
    @Sensitive(strategy = SensitiveStrategy.PHONE)
    private String memberName;

    @ApiModelProperty(value = "店铺名称")
    private String storeName;

    @ApiModelProperty(value = "店铺ID")
    private String storeId;

    /**
     * @see ClientTypeEnum
     */
    @ApiModelProperty(value = "订单来源")
    private String clientType;

    /**
     * 子订单信息
     */
    private List<OrderItemVO> orderItems;

    @ApiModelProperty(hidden = true, value = "item goods_id")
    @Setter
    private String groupGoodsId;

    @ApiModelProperty(hidden = true, value = "item sku id")
    @Setter
    private String groupSkuId;

    @ApiModelProperty(hidden = true, value = "item 数量")
    @Setter
    private String groupNum;

    @ApiModelProperty(hidden = true, value = "item 图片")
    @Setter
    private String groupImages;

    @ApiModelProperty(hidden = true, value = "item 名字")
    @Setter
    private String groupName;

    @ApiModelProperty(hidden = true, value = "item 编号")
    @Setter
    private String groupOrderItemsSn;

    @ApiModelProperty(hidden = true, value = "item 商品价格")
    @Setter
    private String groupGoodsPrice;
    /**
     * @see cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum
     */
    @ApiModelProperty(hidden = true, value = "item 售后状态", allowableValues = "NOT_APPLIED(未申请),ALREADY_APPLIED(已申请),EXPIRED(已失效不允许申请售后)")
    @Setter
    private String groupAfterSaleStatus;

    /**
     * @see cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum
     */
    @ApiModelProperty(hidden = true, value = "item 投诉状态")
    @Setter
    private String groupComplainStatus;

    /**
     * @see cn.lili.modules.order.order.entity.enums.CommentStatusEnum
     */
    @ApiModelProperty(hidden = true, value = "item 评价状态")
    @Setter
    private String groupCommentStatus;


    /**
     * @see cn.lili.modules.order.order.entity.enums.OrderTypeEnum
     */
    @ApiModelProperty(value = "订单类型")
    private String orderType;

    /**
     * @see cn.lili.modules.order.order.entity.enums.DeliverStatusEnum
     */
    @ApiModelProperty(value = "货运状态")
    private String deliverStatus;

    /**
     * @see cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum
     */
    @ApiModelProperty(value = "订单促销类型")
    private String orderPromotionType;

    public List<OrderItemVO> getOrderItems() {
        if (CharSequenceUtil.isEmpty(groupGoodsId)) {
            return new ArrayList<>();
        }
        List<OrderItemVO> orderItemVOS = new ArrayList<>();


        String[] goodsId = groupGoodsId.split(",");

        for (int i = 0; i < goodsId.length; i++) {
            orderItemVOS.add(this.getOrderItemVO(i));
        }
        return orderItemVOS;

    }

    private OrderItemVO getOrderItemVO(int i) {
        OrderItemVO orderItemVO = new OrderItemVO();
        orderItemVO.setGoodsId(groupGoodsId.split(",")[i]);
        if (CharSequenceUtil.isNotEmpty(groupOrderItemsSn)) {
            orderItemVO.setSn(groupOrderItemsSn.split(",")[i]);
        }
        if (CharSequenceUtil.isNotEmpty(groupSkuId)) {
            orderItemVO.setSkuId(groupSkuId.split(",")[i]);
        }
        if (CharSequenceUtil.isNotEmpty(groupName)) {
            orderItemVO.setName(groupName.split(",")[i]);
        }
        if (CharSequenceUtil.isNotEmpty(groupNum) && groupNum.split(",").length == groupGoodsId.split(",").length) {
            orderItemVO.setNum(groupNum.split(",")[i]);
        }
        if (CharSequenceUtil.isNotEmpty(groupImages) && groupImages.split(",").length == groupGoodsId.split(",").length) {
            orderItemVO.setImage(groupImages.split(",")[i]);
        }
        if (CharSequenceUtil.isNotEmpty(groupAfterSaleStatus) && groupAfterSaleStatus.split(",").length == groupGoodsId.split(",").length) {
            orderItemVO.setAfterSaleStatus(groupAfterSaleStatus.split(",")[i]);
        }
        if (CharSequenceUtil.isNotEmpty(groupComplainStatus) && groupComplainStatus.split(",").length == groupGoodsId.split(",").length) {
            orderItemVO.setComplainStatus(groupComplainStatus.split(",")[i]);
        }
        if (CharSequenceUtil.isNotEmpty(groupCommentStatus) && groupCommentStatus.split(",").length == groupGoodsId.split(",").length) {
            orderItemVO.setCommentStatus(groupCommentStatus.split(",")[i]);
        }
        if (CharSequenceUtil.isNotEmpty(groupGoodsPrice) && groupGoodsPrice.split(",").length == groupGoodsId.split(",").length) {
            orderItemVO.setGoodsPrice(Double.parseDouble(groupGoodsPrice.split(",")[i]));
        }
        return orderItemVO;
    }

    /**
     * 初始化自身状态
     */
    public AllowOperation getAllowOperationVO() {
        //设置订单的可操作状态
        return new AllowOperation(this);
    }


}
