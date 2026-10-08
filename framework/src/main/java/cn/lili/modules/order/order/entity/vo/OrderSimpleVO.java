package cn.lili.modules.order.order.entity.vo;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.security.sensitive.Sensitive;
import cn.lili.common.security.sensitive.enums.SensitiveStrategy;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.Setter;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 订单简略信息
 * 用于订单列表查看
 *
 * @author Chopper
 * @since 2020-08-17 20:28
 */
@Data
public class OrderSimpleVO {

    @Schema(description = "sn")
    private String sn;

    @Schema(description = "总价格")
    private Double flowPrice;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "创建时间")
    private Date createTime;

    /**
     * @see cn.lili.modules.order.order.entity.enums.OrderStatusEnum
     */
    @Schema(description = "订单状态")
    private String orderStatus;

    /**
     * @see cn.lili.modules.order.order.entity.enums.PayStatusEnum
     */
    @Schema(description = "付款状态")
    private String payStatus;

    @Schema(description = "支付方式")
    private String paymentMethod;

    @Schema(description = "支付时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date paymentTime;

    @Schema(description = "用户ID")
    private String memberId;

    @Schema(description = "用户名")
    @Sensitive(strategy = SensitiveStrategy.PHONE)
    private String memberName;

    @Schema(description = "收货人姓名")
    private String consigneeName;

    @Schema(description = "收货人手机")
    private String consigneeMobile;

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "店铺ID")
    private String storeId;

    /**
     * @see ClientTypeEnum
     */
    @Schema(description = "订单来源")
    private String clientType;

    /**
     * 子订单信息
     */
    private List<OrderItemVO> orderItems;

    @Schema(hidden = true, description = "item goods_id")
    @Setter
    private String groupGoodsId;

    @Schema(hidden = true, description = "item sku id")
    @Setter
    private String groupSkuId;

    @Schema(hidden = true, description = "item 数量")
    @Setter
    private String groupNum;

    @Schema(hidden = true, description = "item 图片")
    @Setter
    private String groupImages;

    @Schema(hidden = true, description = "item 名字")
    @Setter
    private String groupName;

    @Schema(hidden = true, description = "item 规格json")
    @Setter
    private String groupSpecs;

    @Schema(hidden = true, description = "item 编号")
    @Setter
    private String groupOrderItemsSn;

    @Schema(hidden = true, description = "item 商品价格")
    @Setter
    private String groupGoodsPrice;
    /**
     * @see cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum
     */
    @Schema(hidden = true, description = "item 售后状态", allowableValues = "NOT_APPLIED(未申请),ALREADY_APPLIED(已申请),EXPIRED(已失效不允许申请售后)")
    @Setter
    private String groupAfterSaleStatus;

    /**
     * @see cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum
     */
    @Schema(hidden = true, description = "item 投诉状态")
    @Setter
    private String groupComplainStatus;

    /**
     * @see cn.lili.modules.order.order.entity.enums.CommentStatusEnum
     */
    @Schema(hidden = true, description = "item 评价状态")
    @Setter
    private String groupCommentStatus;


    /**
     * @see cn.lili.modules.order.order.entity.enums.OrderTypeEnum
     */
    @Schema(description = "订单类型")
    private String orderType;

    /**
     * @see cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum
     */
    @Schema(description = "配送方式")
    private String deliveryMethod;

    /**
     * @see cn.lili.modules.order.order.entity.enums.DeliverStatusEnum
     */
    @Schema(description = "货运状态")
    private String deliverStatus;

    /**
     * @see cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum
     */
    @Schema(description = "订单促销类型")
    private String orderPromotionType;

    @Schema(description = "是否退款")
    private String groupIsRefund;

    @Schema(description = "退款金额")
    private String groupRefundPrice;

    @Schema(description = "卖家订单备注")
    private String sellerRemark;

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
        if (CharSequenceUtil.isNotEmpty(groupSpecs)) {
            String[] specs = groupSpecs.split("\u001e", -1);
            if (specs.length == groupGoodsId.split(",").length) {
                orderItemVO.setSpecs(formatSpecs(specs[i]));
            }
        }
        if (CharSequenceUtil.isNotEmpty(groupNum) && groupNum.split(",").length == groupGoodsId.split(",").length) {
            orderItemVO.setNum(groupNum.split(",")[i]);
        }
        if (CharSequenceUtil.isNotEmpty(groupImages) && groupImages.split(",").length == groupGoodsId.split(",").length) {
            orderItemVO.setImage(groupImages.split(",")[i]);
        }
        if (CharSequenceUtil.isNotEmpty(groupAfterSaleStatus) && groupAfterSaleStatus.split(",").length == groupGoodsId.split(",").length) {
            if (!OrderPromotionTypeEnum.isCanAfterSale(this.orderPromotionType)) {
                orderItemVO.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.EXPIRED.name());
            } else {
                orderItemVO.setAfterSaleStatus(groupAfterSaleStatus.split(",")[i]);
            }
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
        if (CharSequenceUtil.isNotEmpty(groupIsRefund) && groupIsRefund.split(",").length == groupGoodsId.split(",").length) {
            orderItemVO.setIsRefund(groupIsRefund.split(",")[i]);
        }
        if (CharSequenceUtil.isNotEmpty(groupRefundPrice) && groupRefundPrice.split(",").length == groupGoodsId.split(",").length) {
            orderItemVO.setRefundPrice(groupRefundPrice.split(",")[i]);
        }
        return orderItemVO;
    }

    /**
     * 规格 JSON 转成列表展示文案，跳过相册等非文字字段。
     */
    private String formatSpecs(String specs) {
        if (CharSequenceUtil.isBlank(specs)) {
            return "";
        }
        String trimmed = specs.trim();
        if (!trimmed.startsWith("{")) {
            return trimmed;
        }
        try {
            JSONObject obj = JSONUtil.parseObj(trimmed);
            StringBuilder text = new StringBuilder();
            for (Map.Entry<String, Object> entry : obj.entrySet()) {
                if ("images".equals(entry.getKey()) || entry.getValue() == null) {
                    continue;
                }
                Object value = entry.getValue();
                if (value instanceof JSONObject || value instanceof JSONArray) {
                    continue;
                }
                String specValue = String.valueOf(value).trim();
                if (specValue.isEmpty()) {
                    continue;
                }
                if (text.length() > 0) {
                    text.append(' ');
                }
                text.append(entry.getKey()).append('：').append(specValue);
            }
            return text.toString();
        } catch (Exception ignored) {
            return trimmed;
        }
    }

    /**
     * 初始化自身状态
     */
    public AllowOperation getAllowOperationVO() {
        //设置订单的可操作状态
        return new AllowOperation(this);
    }

    public String getGroupAfterSaleStatus() {
        // 不可售后的订单类型集合
        if (!OrderPromotionTypeEnum.isCanAfterSale(this.orderPromotionType)) {
            return OrderItemAfterSaleStatusEnum.EXPIRED.name();
        }
        return groupAfterSaleStatus;
    }
}
