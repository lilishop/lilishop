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

    /**
     * 修改分销员可提现金额
     *
     * @param commissionFrozen      分销金额
     * @param distributionId 分销员ID
     */
    @Update("UPDATE li_distribution set commission_frozen = (IFNULL(commission_frozen,0)+#{commissionFrozen}) " +
            ", rebate_total=(IFNULL(rebate_total,0)+#{commissionFrozen})  WHERE id = #{distributionId}")
    void subCanRebate(Double commissionFrozen, String distributionId);

    /**
     * 添加分销金额
     *
     * @param commissionFrozen      分销金额
     * @param distributionId 分销员ID
     */
    @Update("UPDATE li_distribution set commission_frozen = (IFNULL(commission_frozen,0)+#{commissionFrozen}) " +
            ", rebate_total=(IFNULL(rebate_total,0)+#{commissionFrozen}) " +
            ", distribution_order_count=(IFNULL(distribution_order_count,0)+1) WHERE id = #{distributionId}")
    void addCanRebate(Double commissionFrozen, String distributionId);

}