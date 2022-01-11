package cn.lili.modules.order.aftersale.entity.vo;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.vo.PageVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 售后搜索参数
 *
 * @author paulG
 * @since 2020/12/4
 **/
@EqualsAndHashCode(callSuper = true)
@Data
public class AfterSaleSearchParams extends PageVO {

    @ApiModelProperty(value = "售后服务单号")
    private String sn;

    @ApiModelProperty(value = "订单编号")
    private String orderSn;

    @ApiModelProperty(value = "会员名称")
    private String memberName;

    @ApiModelProperty(value = "商家名称")
    private String storeName;

    @ApiModelProperty(value = "商家ID")
    private String storeId;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "申请退款金额,可以为范围，如10_1000")
    private String applyRefundPrice;

    @ApiModelProperty(value = "实际退款金额,可以为范围，如10_1000")
    private String actualRefundPrice;

    @ApiModelProperty(value = "总价格,可以为范围，如10_1000")
    private String flowPrice;

    /**
     * @see cn.lili.modules.order.trade.entity.enums.AfterSaleTypeEnum
     */
    @ApiModelProperty(value = "售后类型", allowableValues = "CANCEL,RETURN_GOODS,EXCHANGE_GOODS,REISSUE_GOODS")
    private String serviceType;

    /**
     * @see cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum
     */
    @ApiModelProperty(value = "售后单状态", allowableValues = "APPLY,PASS,REFUSE,BUYER_RETURN,SELLER_RE_DELIVERY,BUYER_CONFIRM,SELLER_CONFIRM,COMPLETE")
    private String serviceStatus;

    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "开始时间")
    private Date startDate;

    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "结束时间")
    private Date endDate;

    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        if (CharSequenceUtil.isNotEmpty(sn)) {
            queryWrapper.like("sn", sn);
        }
        if (CharSequenceUtil.isNotEmpty(orderSn)) {
            queryWrapper.like("order_sn", orderSn);
        }
        //按买家查询
        if (CharSequenceUtil.equals(UserContext.getCurrentUser().getRole().name(), UserEnums.MEMBER.name())) {
            queryWrapper.eq("member_id", UserContext.getCurrentUser().getId());
        }
        //按卖家查询
        if (CharSequenceUtil.equals(UserContext.getCurrentUser().getRole().name(), UserEnums.STORE.name())) {
            queryWrapper.eq("store_id", UserContext.getCurrentUser().getStoreId());
        }

        if (CharSequenceUtil.equals(UserContext.getCurrentUser().getRole().name(), UserEnums.MANAGER.name())
                && CharSequenceUtil.isNotEmpty(storeId)
        ) {
            queryWrapper.eq("store_id", storeId);
        }
        if (CharSequenceUtil.isNotEmpty(memberName)) {
            queryWrapper.like("member_name", memberName);
        }
        if (CharSequenceUtil.isNotEmpty(storeName)) {
            queryWrapper.like("store_name", storeName);
        }
        if (CharSequenceUtil.isNotEmpty(goodsName)) {
            queryWrapper.like("goods_name", goodsName);
        }
        //按时间查询
        if (startDate != null) {
            queryWrapper.ge("create_time", startDate);
        }
        if (endDate != null) {
            queryWrapper.le("create_time", endDate);
        }
        if (CharSequenceUtil.isNotEmpty(serviceStatus)) {
            queryWrapper.eq("service_status", serviceStatus);
        }
        if (CharSequenceUtil.isNotEmpty(serviceType)) {
            queryWrapper.eq("service_type", serviceType);
        }
        this.betweenWrapper(queryWrapper);
        queryWrapper.eq("delete_flag", false);
        return queryWrapper;
    }

    private <T> void betweenWrapper(QueryWrapper<T> queryWrapper) {
        if (CharSequenceUtil.isNotEmpty(applyRefundPrice)) {
            String[] s = applyRefundPrice.split("_");
            if (s.length > 1) {
                queryWrapper.between("apply_refund_price", s[0], s[1]);
            } else {
                queryWrapper.ge("apply_refund_price", s[0]);
            }
        }
        if (CharSequenceUtil.isNotEmpty(actualRefundPrice)) {
            String[] s = actualRefundPrice.split("_");
            if (s.length > 1) {
                queryWrapper.between("actual_refund_price", s[0], s[1]);
            } else {
                queryWrapper.ge("actual_refund_price", s[0]);
            }
        }
        if (CharSequenceUtil.isNotEmpty(flowPrice)) {
            String[] s = flowPrice.split("_");
            if (s.length > 1) {
                queryWrapper.between("flow_price", s[0], s[1]);
            } else {
                queryWrapper.ge("flow_price", s[0]);
            }
        }
    }


}
