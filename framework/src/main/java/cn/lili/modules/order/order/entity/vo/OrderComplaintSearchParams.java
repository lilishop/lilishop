package cn.lili.modules.order.order.entity.vo;

import cn.hutool.core.util.StrUtil;
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
        if (StrUtil.isNotEmpty(status)) {
            queryWrapper.eq(OrderComplaint::getComplainStatus, status);
        }
        if (StrUtil.isNotEmpty(orderSn)) {
            queryWrapper.eq(OrderComplaint::getOrderSn, orderSn);
        }
        if (StrUtil.isNotEmpty(storeName)) {
            queryWrapper.like(OrderComplaint::getStoreName, storeName);
        }
        if (StrUtil.isNotEmpty(storeId)) {
            queryWrapper.eq(OrderComplaint::getStoreId, storeId);
        }
        if (StrUtil.isNotEmpty(memberName)) {
            queryWrapper.like(OrderComplaint::getMemberName, memberName);
        }
        if (StrUtil.isNotEmpty(memberId)) {
            queryWrapper.eq(OrderComplaint::getMemberId, memberId);
        }
        queryWrapper.eq(OrderComplaint::getDeleteFlag, false);
        queryWrapper.orderByDesc(OrderComplaint::getCreateTime);
        return queryWrapper;
    }


}
