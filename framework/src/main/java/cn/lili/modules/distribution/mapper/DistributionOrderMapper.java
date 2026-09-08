package cn.lili.modules.distribution.mapper;

import cn.hutool.core.date.DateTime;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Update;

import java.util.Date;

/**
 * 分销订单数据处理层
 *
 * @author pikachu
 * @since 2020-03-15 10:45:56
 */
public interface DistributionOrderMapper extends BaseMapper<DistributionOrder> {

    /**
     * 批量解冻到期分销订单佣金到分销员可提现余额。
     *
     * @param distributionOrderStatus 待解冻的订单状态
     * @param settleCycle             解冻截止时间
     */
    @Update("UPDATE li_distribution AS d "
            + "SET d.can_rebate = (IFNULL(d.can_rebate, 0) + ("
            + "  SELECT SUM(dorder.rebate) FROM li_distribution_order AS dorder "
            + "  WHERE dorder.distribution_order_status = #{distributionOrderStatus} "
            + "    AND dorder.settle_cycle < #{settleCycle} "
            + "    AND dorder.distribution_id = d.id"
            + ")), "
            + "d.commission_frozen = (IFNULL(d.commission_frozen, 0) - ("
            + "  SELECT SUM(dorder.rebate) FROM li_distribution_order AS dorder "
            + "  WHERE dorder.distribution_order_status = #{distributionOrderStatus} "
            + "    AND dorder.settle_cycle < #{settleCycle} "
            + "    AND dorder.distribution_id = d.id"
            + "))")
    void rebate(String distributionOrderStatus, DateTime settleCycle);

    /**
     * 将已过售后期且未结算的分销订单标记为待结算，并写入解冻周期。
     *
     * @param settleCycle 解冻周期时间
     * @return 影响行数
     */
    @Update("UPDATE li_distribution_order distribution_order "
            + "INNER JOIN li_order_item oi ON oi.sn = distribution_order.order_item_sn "
            + "SET distribution_order.distribution_order_status = 'WAIT_BILL', "
            + "distribution_order.settle_cycle = #{settleCycle} "
            + "WHERE distribution_order.distribution_order_status = 'NO_COMPLETED' "
            + "AND oi.after_sale_status = 'EXPIRED'")
    int distributionSettlementOrder(Date settleCycle);
}
