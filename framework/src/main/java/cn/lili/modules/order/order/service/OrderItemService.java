package cn.lili.modules.order.order.service;

import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.CommentStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 子订单业务层
 *
 * @author Chopper
 * @since 2020/11/17 7:36 下午
 */
public interface OrderItemService extends IService<OrderItem> {

    /**
     * 更新评论状态
     *
     * @param orderItemSn       子订单编号
     * @param commentStatusEnum 评论状态枚举
     */
    void updateCommentStatus(String orderItemSn, CommentStatusEnum commentStatusEnum);

    /**
     * 更新可申请售后状态
     *
     * @param orderItemSn                  子订单编号
     * @param orderItemAfterSaleStatusEnum 售后状态枚举
     */
    void updateAfterSaleStatus(String orderItemSn, OrderItemAfterSaleStatusEnum orderItemAfterSaleStatusEnum);

    /**
     * 更新订单可投诉状态
     *
     * @param orderSn            订单sn
     * @param skuId              商品skuId
     * @param complainId         订单交易投诉ID
     * @param complainStatusEnum 修改状态
     */
    void updateOrderItemsComplainStatus(String orderSn, String skuId, String complainId, OrderComplaintStatusEnum complainStatusEnum);

    /**
     * 根据子订单编号获取子订单信息
     *
     * @param sn 子订单编号
     * @return 子订单
     */
    OrderItem getBySn(String sn);

    /**
     * 根据订单编号获取子订单列表
     *
     * @param orderSn 订单编号
     * @return 子订单列表
     */
    List<OrderItem> getByOrderSn(String orderSn);

    /**
     * 子订单查询
     *
     * @param orderSn 订单编号
     * @param skuId   skuid
     * @return 子订单
     */
    OrderItem getByOrderSnAndSkuId(String orderSn, String skuId);
}