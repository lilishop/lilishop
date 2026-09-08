package cn.lili.modules.distribution.serviceimpl;

import cn.lili.common.utils.StringUtils;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.enums.DistributionOrderStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionOrderSearchParams;
import cn.lili.modules.distribution.mapper.DistributionOrderMapper;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.distribution.service.DistributionRewardService;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 分销订单接口实现
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@Slf4j
@Service
public class DistributionOrderServiceImpl extends ServiceImpl<DistributionOrderMapper, DistributionOrder>
        implements DistributionOrderService {

    /**
     * 分销奖励服务（支付入账、结算、冲回均委托于此）。
     */
    @Autowired
    private DistributionRewardService distributionRewardService;

    /**
     * 分页查询分销订单。走 MP {@code page}，以 {@code rebate} 正确映射到 {@code directCommission}。
     *
     * @param distributionOrderSearchParams 查询参数
     * @return 分销订单分页
     */
    @Override
    public IPage<DistributionOrder> getDistributionOrderPage(
            DistributionOrderSearchParams distributionOrderSearchParams) {
        if (StringUtils.isBlank(distributionOrderSearchParams.getSort())) {
            distributionOrderSearchParams.setSort("create_time");
        }
        return this.page(PageUtil.initPage(distributionOrderSearchParams),
                distributionOrderSearchParams.queryWrapper());
    }

    /**
     * 订单取消：释放冻结奖励并标记分销订单为已退款。
     *
     * @param orderSn 订单编号
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void cancelOrder(String orderSn) {
        // 释放该订单下冻结的奖励，并将分销订单标记为已退款
        distributionRewardService.cancelRewardsByOrderSn(orderSn);
        this.update(new LambdaUpdateWrapper<DistributionOrder>().eq(DistributionOrder::getOrderSn, orderSn)
                .set(DistributionOrder::getDistributionOrderStatus, DistributionOrderStatusEnum.REFUND.name()));
    }

    /**
     * 售后完成时冲回分销佣金。
     *
     * @param afterSale 售后单
     */
    @Override
    public void refundOrder(AfterSale afterSale) {
        if (afterSale == null || afterSale.getOrderItemSn() == null) {
            return;
        }
        // 按订单明细冲回已冻结/已结算的佣金
        distributionRewardService.refundRewardsByOrderItem(afterSale.getOrderItemSn());
    }

    /**
     * 订单完成时结算分销佣金。
     *
     * @param storeFlow 店铺流水
     */
    @Override
    public void completeOrder(StoreFlow storeFlow) {
        if (storeFlow == null || storeFlow.getOrderItemSn() == null) {
            return;
        }
        String orderItemSn = storeFlow.getOrderItemSn();
        // 无分销奖励记录则无需结算
        if (!distributionRewardService.hasRewardsByOrderItemSn(orderItemSn)) {
            return;
        }
        // 结算佣金并将分销订单从未完成更新为已完成
        distributionRewardService.settleRewardsByOrderItem(orderItemSn);
        this.update(new LambdaUpdateWrapper<DistributionOrder>()
                .eq(DistributionOrder::getOrderItemSn, orderItemSn)
                .eq(DistributionOrder::getDistributionOrderStatus, DistributionOrderStatusEnum.NO_COMPLETED.name())
                .set(DistributionOrder::getDistributionOrderStatus, DistributionOrderStatusEnum.COMPLETE.name()));
    }

    /**
     * 支付成功后处理分销奖励入账（委托 V2 奖励服务）。
     *
     * @param orderSn 订单编号
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void handlePaymentRewards(String orderSn) {
        // 委托 V2 奖励服务（写单、冻结、流水均在其内幂等完成）
        distributionRewardService.handlePaymentRewards(orderSn);
    }
}
