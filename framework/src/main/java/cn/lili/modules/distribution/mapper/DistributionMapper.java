package cn.lili.modules.distribution.mapper;

import cn.lili.modules.distribution.entity.dos.Distribution;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Update;


/**
 * 分销员数据处理层
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
public interface DistributionMapper extends BaseMapper<Distribution> {

    @Update("UPDATE li_distribution set commission_frozen = (IFNULL(commission_frozen,0) - #{commissionFrozen}) " +
            ", rebate_total=(IFNULL(rebate_total,0) - #{commissionFrozen}) " +
            ", distribution_order_count=(IFNULL(distribution_order_count,0)-1) " +
            " WHERE id = #{distributionId}")
    void subRebate(Double commissionFrozen, String distributionId, Double distributionOrderPrice);

    @Update("UPDATE li_distribution set commission_frozen = (IFNULL(commission_frozen,0)+#{commissionFrozen}) " +
            ", rebate_total=(IFNULL(rebate_total,0)+#{commissionFrozen}) " +
            ", distribution_order_price=(IFNULL(distribution_order_price,0)+#{distributionOrderPrice}) " +
            ", distribution_order_count=(IFNULL(distribution_order_count,0)+1) " +
            " WHERE id = #{distributionId}")
    void addRebate(Double commissionFrozen, String distributionId, Double distributionOrderPrice);


    @Update("UPDATE li_distribution SET commission_frozen = (IFNULL(commission_frozen,0) - #{rebate}) " +
            ",can_rebate=(IFNULL(can_rebate,0) + #{rebate}) " +
            " WHERE id = #{distributionId}")
    void addCanRebate(Double rebate, String distributionId);

    @Update("UPDATE li_distribution SET can_rebate=(IFNULL(can_rebate,0) - #{rebate}),cash_rebate=(IFNULL(cash_rebate,0) + #{rebate}) " +
            " WHERE id = #{distributionId}")
    void addCashRebate(Double rebate, String distributionId);


    @Update("UPDATE li_distribution SET cash_rebate=(IFNULL(cash_rebate,0) - #{rebate}) " +
            " WHERE id = #{distributionId}")
    void subCashRebate(Double rebate, String distributionId);

}