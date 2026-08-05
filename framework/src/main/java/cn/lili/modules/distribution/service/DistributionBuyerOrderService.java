package cn.lili.modules.distribution.service;

import cn.lili.modules.distribution.entity.dto.DistributionBuyerOrderQueryDTO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerOrderDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerOrderPageVO;

/**
 * 分销员推广/邀请订单查询
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionBuyerOrderService {

    /**
     * 分页查询推广或邀请订单。
     *
     * @param distributionId 分销员ID
     * @param query          查询参数
     * @return 分页结果
     */
    DistributionBuyerOrderPageVO pageOrders(String distributionId, DistributionBuyerOrderQueryDTO query);

    /**
     * 查询推广/邀请订单详情。
     *
     * @param distributionId 分销员ID
     * @param orderItemSn    订单明细编号
     * @param orderType      订单类型 PROMOTION/INVITE
     * @return 订单详情
     */
    DistributionBuyerOrderDetailVO getOrderDetail(String distributionId, String orderItemSn, String orderType);
}
