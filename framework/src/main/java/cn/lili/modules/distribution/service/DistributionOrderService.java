package cn.lili.modules.distribution.service;

import cn.hutool.core.date.DateTime;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.vos.DistributionOrderSearchParams;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;


/**
 * 分销订单业务层
 *
 * @author pikachu
 * @since 2020-03-15 10:46:33
 */
public interface DistributionOrderService extends IService<DistributionOrder> {

    /**
     * 获取分销订单分页
     * @param distributionOrderSearchParams 分销订单搜索参数
     * @return 分销订单分页
     */
    IPage<DistributionOrder> getDistributionOrderPage(DistributionOrderSearchParams distributionOrderSearchParams);

    /**
     * 支付订单
     * 记录分销订单
     *
     * @param orderSn 订单编号
     */
    void calculationDistribution(String orderSn);

    /**
     * 取消订单
     * 记录分销订单
     *
     * @param orderSn 订单编号
     */
    void cancelOrder(String orderSn);

    /**
     * 订单退款
     * 记录分销订单
     *
     * @param afterSaleSn 售后单号
     */
    void refundOrder(AfterSale afterSale);

    /**
     * 分销订单状态修改
     *
     * @param orderItems
     */
    void updateDistributionOrderStatus(List<OrderItem> orderItems);

    /**
     * 分销订单结算
     * @param dateTime
     * @param distributionOrderStatus
     */
    void updateRebate(DateTime dateTime, String distributionOrderStatus);
}