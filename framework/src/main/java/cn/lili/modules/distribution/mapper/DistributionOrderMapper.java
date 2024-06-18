package cn.lili.modules.distribution.mapper;

import cn.hutool.core.date.DateTime;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import java.util.Date;
import java.util.List;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;

/**
 * 分销订单数据处理层
 *
 * @author pikachu
 * @since 2020-03-15 10:45:56
 */
public interface DistributionOrderMapper extends BaseMapper<DistributionOrder> {

    /**
     * 修改分销员提现金额
     *
     * @param distributionOrderStatus 分销订单状态
     * @param settleCycle             时间
     */
    @Update("UPDATE li_distribution AS d " +
            "SET d.can_rebate =(ifnull(d.can_rebate,0) +(SELECT SUM( dorder.rebate ) FROM li_distribution_order AS dorder WHERE dorder.distribution_order_status = #{distributionOrderStatus} AND dorder.settle_cycle< #{settleCycle} AND dorder.distribution_id = d.id ))" +
            ",d.commission_frozen =(ifnull(d.commission_frozen,0) -(SELECT SUM( dorder.rebate ) FROM li_distribution_order AS dorder WHERE dorder.distribution_order_status = #{distributionOrderStatus} AND dorder.settle_cycle< #{settleCycle} AND dorder.distribution_id = d.id ) )")
    void rebate(String distributionOrderStatus, DateTime settleCycle);

    /**
     * 查询待结算的分销订单
     * @return 待结算的分销订单
     */
    @Select("UPDATE li_distribution_order distribution_order"
        + " INNER JOIN li_order_item oi ON oi.sn = distribution_order.order_item_sn"
        + " SET distribution_order.distribution_order_status = 'WAIT_BILL',"
        + "    distribution_order.settle_cycle = #{settleCycle} "
        + " WHERE distribution_order.distribution_order_status = 'NO_COMPLETED'"
        + "  AND oi.after_sale_status = 'EXPIRED';")
    List<DistributionOrder> distributionSettlementOrder(Date settleCycle);
}
