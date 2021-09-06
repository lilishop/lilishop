package cn.lili.modules.order.order.entity.vo;

import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.order.order.entity.enums.*;
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
     * @see OrderStatusEnum
     */
    @ApiModelProperty(value = "订单状态")
    private String orderStatus;

    /**
     * @see PayStatusEnum
     */
    @ApiModelProperty(value = "付款状态")
    private String payStatus;

    @ApiModelProperty(value = "支付方式")
    private String paymentMethod;

    @ApiModelProperty(value = "支付时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date paymentTime;

    @ApiModelProperty(value = "用户名")
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
     * @see OrderItemAfterSaleStatusEnum
     */
    @ApiModelProperty(hidden = true, value = "item 售后状态", allowableValues = "NOT_APPLIED(未申请),ALREADY_APPLIED(已申请),EXPIRED(已失效不允许申请售后)")
    @Setter
    private String groupAfterSaleStatus;

    /**
     * @see OrderComplaintStatusEnum
     */
    @ApiModelProperty(hidden = true, value = "item 投诉状态")
    @Setter
    private String groupComplainStatus;

    /**
     * @see CommentStatusEnum
     */
    @ApiModelProperty(hidden = true, value = "item 评价状态")
    @Setter
    private String groupCommentStatus;


    /**
     * @see OrderTypeEnum
     */
    @ApiModelProperty(value = "订单类型")
    private String orderType;

    /**
     * @see DeliverStatusEnum
     */
    @ApiModelProperty(value = "货运状态")
    private String deliverStatus;

    public List<OrderItemVO> getOrderItems() {
        if (StringUtils.isEmpty(groupGoodsId)) {
            return new ArrayList<>();
        }
        List<OrderItemVO> orderItemVOS = new ArrayList<>();
        String[] orderItemsSn = groupOrderItemsSn.split(",");
        String[] goodsId = groupGoodsId.split(",");
        String[] skuId = groupSkuId.split(",");
        String[] num = groupNum.split(",");
        String[] image = groupImages.split(",");
        String[] name = groupName.split(",");
        String[] afterSaleStatus = groupAfterSaleStatus.split(",");
        String[] complainStatus = groupComplainStatus.split(",");
        String[] commentStatus = groupCommentStatus.split(",");
        String[] goodsPrice = groupGoodsPrice.split(",");

        for (int i = 0; i < goodsId.length; i++) {
            orderItemVOS.add(new OrderItemVO(orderItemsSn[i], goodsId[i], skuId[i], num[i], image[i], name[i], afterSaleStatus[i], complainStatus[i], commentStatus[i], Double.parseDouble(goodsPrice[i])));
        }
        return orderItemVOS;

    }

    /**
     * 初始化自身状态
     */
    public AllowOperation getAllowOperationVO() {
        //设置订单的可操作状态
        return new AllowOperation(this);
    }


}
