package cn.lili.modules.distribution.service;

import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.vos.DistributionOrderSearchParams;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 分销订单业务层。
 * <p>
 * 记录分销销售归因事实，并在订单支付、完成、取消、退款等节点
 * 驱动佣金冻结、结算与 V2 二级奖励计算。
 * </p>
 *
 * @author pikachu
 * @since 2020-03-15
 */
public interface DistributionOrderService extends IService<DistributionOrder> {

    /**
     * 分页查询分销订单。
     *
     * @param distributionOrderSearchParams 搜索条件
     * @return 分销订单分页
     */
    IPage<DistributionOrder> getDistributionOrderPage(DistributionOrderSearchParams distributionOrderSearchParams);

    /**
     * 订单取消时回滚分销订单及冻结佣金。
     *
     * @param orderSn 订单编号
     */
    void cancelOrder(String orderSn);

    /**
     * 售后退款时回滚分销订单佣金。
     *
     * @param afterSale 售后单
     */
    void refundOrder(AfterSale afterSale);

    /**
     * 订单完成时结算分销佣金。
     *
     * @param storeFlow 店铺流水
     */
    void completeOrder(StoreFlow storeFlow);

    /**
     * 订单支付成功后处理分销奖励入账（直属佣金 + 邀请奖励）。
     * <p>
     * 支付成功事件与待发货等补偿节点均应调用此方法；内部具备幂等与按奖励类型补齐能力。
     * </p>
     *
     * @param orderSn 订单编号
     */
    void handlePaymentRewards(String orderSn);

}
