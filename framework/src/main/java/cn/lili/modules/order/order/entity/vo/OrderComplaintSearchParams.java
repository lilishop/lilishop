package cn.lili.modules.order.order.entity.vo;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.order.aftersale.entity.enums.ComplaintStatusEnum;
import cn.lili.modules.order.order.entity.dos.OrderComplaint;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 订单投诉查询参数
 *
 * @author paulG
 * @since 2020/12/4
 **/
@Data
public class OrderComplaintSearchParams {

    /**
     * @see ComplaintStatusEnum
     */
    @ApiModelProperty(value = "交易投诉状态")
    private String status;

    @ApiModelProperty(value = "订单号")
    private String orderSn;

    @ApiModelProperty(value = "会员id")
    private String memberId;

    @ApiModelProperty(value = "会员名称")
    private String memberName;

    @ApiModelProperty(value = "商家id")
    private String storeId;

    @ApiModelProperty(value = "商家名称")
    private String storeName;

    public LambdaQueryWrapper<OrderComplaint> lambdaQueryWrapper() {
        LambdaQueryWrapper<OrderComplaint> queryWrapper = new LambdaQueryWrapper<>();
        if (CharSequenceUtil.isNotEmpty(status)) {
            queryWrapper.eq(OrderComplaint::getComplainStatus, status);
        }
        if (CharSequenceUtil.isNotEmpty(orderSn)) {
            queryWrapper.like(OrderComplaint::getOrderSn, orderSn);
        }
        if (CharSequenceUtil.isNotEmpty(storeName)) {
            queryWrapper.like(OrderComplaint::getStoreName, storeName);
        }
        if (CharSequenceUtil.isNotEmpty(storeId)) {
            queryWrapper.eq(OrderComplaint::getStoreId, storeId);
        }
        if (CharSequenceUtil.isNotEmpty(memberName)) {
            queryWrapper.like(OrderComplaint::getMemberName, memberName);
        }
        if (CharSequenceUtil.isNotEmpty(memberId)) {
            queryWrapper.eq(OrderComplaint::getMemberId, memberId);
        }
        queryWrapper.eq(OrderComplaint::getDeleteFlag, false);
        queryWrapper.orderByDesc(OrderComplaint::getCreateTime);
        return queryWrapper;
    }


}
