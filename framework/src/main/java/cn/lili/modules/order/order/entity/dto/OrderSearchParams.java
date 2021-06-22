package cn.lili.modules.order.order.entity.dto;

import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTagEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 订单查询参数
 *
 * @author Chopper
 * @date 2020/11/17 4:33 下午
 */
@Data
public class OrderSearchParams extends PageVO {

    private static final long serialVersionUID = -6380573339089959194L;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "订单编号")
    private String orderSn;

    @ApiModelProperty(value = "页面标签",
            example = "ALL:全部," +
                    "WAIT_PAY:待付款," +
                    "WAIT_ROG:待收货," +
                    "CANCELLED:已取消," +
                    "COMPLETE:已完成")
    private String tag;

    @ApiModelProperty(value = "商家ID")
    private String storeId;

    @ApiModelProperty(value = "会员ID")
    private String memberId;

    @ApiModelProperty(value = "收货人")
    private String shipName;

    @ApiModelProperty(value = "买家昵称")
    private String buyerName;

    @ApiModelProperty(value = "订单状态")
    private String orderStatus;

    @ApiModelProperty(value = "付款状态")
    private String payStatus;

    @ApiModelProperty(value = "关键字 商品名称/买家名称/店铺名称")
    private String keywords;

    @ApiModelProperty(value = "付款方式")
    private String paymentType;

    /**
     * @see OrderTypeEnum
     * @see cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum
     */
    @ApiModelProperty(value = "订单类型", allowableValues = "NORMAL,VIRTUAL,GIFT,PINTUAN,POINT")
    private String orderType;

    @ApiModelProperty(value = "支付方式")
    private String paymentMethod;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "支付时间")
    private Date paymentTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @ApiModelProperty(value = "下单开始时间")
    private Date startDate;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @ApiModelProperty(value = "下单结束时间")
    private Date endDate;

    @ApiModelProperty(value = "订单来源")
    private String clientType;

    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> wrapper = new QueryWrapper<>();

        //关键字查询
        if (StringUtils.isNotEmpty(keywords)) {
            wrapper.and(queryWrapper -> wrapper.like("o.sn", keywords).or().
                    like("oi.goods_name", keywords));
        }
        //按卖家查询
        wrapper.eq(StringUtils.equals(UserContext.getCurrentUser().getRole().name(), UserEnums.STORE.name()), "o.store_id", UserContext.getCurrentUser().getStoreId());

        //店铺查询
        wrapper.eq(StringUtils.equals(UserContext.getCurrentUser().getRole().name(), UserEnums.MANAGER.name())
                && StringUtils.isNotEmpty(storeId), "o.store_id", storeId);

        //按买家查询
        wrapper.eq(StringUtils.equals(UserContext.getCurrentUser().getRole().name(), UserEnums.MEMBER.name()), "o.member_id", UserContext.getCurrentUser().getId());

        //按照买家查询
        wrapper.like(StringUtils.isNotEmpty(memberId), "o.member_id", memberId);

        //按订单编号查询
        wrapper.like(StringUtils.isNotEmpty(orderSn), "o.sn", orderSn);

        //按时间查询
        wrapper.ge(startDate != null, "o.create_time", startDate);

        wrapper.le(endDate != null, "o.create_time", DateUtil.endOfDate(endDate));
        //按购买人用户名
        wrapper.like(StringUtils.isNotEmpty(buyerName), "o.member_name", buyerName);

        //按订单类型
        if (StringUtils.isNotEmpty(orderType)) {
            wrapper.and(queryWrapper-> queryWrapper.eq("o.order_type", orderType).or()
                    .eq("o.order_promotion_type", orderType));
        }


        //物流查询
        wrapper.like(StringUtils.isNotEmpty(shipName), "o.ship_name", shipName);

        //按商品名称查询
        wrapper.like(StringUtils.isNotEmpty(goodsName), "oi.goods_name", goodsName);

        //付款方式
        wrapper.like(StringUtils.isNotEmpty(paymentType), "o.payment_type", paymentType);

        //按支付方式
        wrapper.eq(StringUtils.isNotEmpty(paymentMethod), "o.payment_method", paymentMethod);

        //订单状态
        wrapper.eq(StringUtils.isNotEmpty(orderStatus), "o.order_status", orderStatus);

        //付款状态
        wrapper.eq(StringUtils.isNotEmpty(payStatus), "o.pay_status", payStatus);

        //订单来源
        wrapper.like(StringUtils.isNotEmpty(clientType), "o.client_type", clientType);


        //按标签查询
        if (StringUtils.isNotEmpty(tag)) {
            String orderStatusColumn = "o.order_status";
            OrderTagEnum tagEnum = OrderTagEnum.valueOf(tag);
            switch (tagEnum) {
                //待付款
                case WAIT_PAY:
                    wrapper.eq(orderStatusColumn, OrderStatusEnum.UNPAID.name());
                    break;
                //待发货
                case WAIT_SHIP:
                    wrapper.eq(orderStatusColumn, OrderStatusEnum.UNDELIVERED.name());
                    break;
                //待收货
                case WAIT_ROG:
                    wrapper.eq(orderStatusColumn, OrderStatusEnum.DELIVERED.name());
                    break;
                //已取消
                case CANCELLED:
                    wrapper.eq(orderStatusColumn, OrderStatusEnum.CANCELLED.name());
                    break;
                //已完成
                case COMPLETE:
                    wrapper.eq(orderStatusColumn, OrderStatusEnum.COMPLETED.name());
                    break;
                default:
                    break;
            }
        }


        wrapper.eq("o.delete_flag", false);
        return wrapper;
    }

}
