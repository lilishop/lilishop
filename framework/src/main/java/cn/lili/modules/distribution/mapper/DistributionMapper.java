package cn.lili.modules.distribution.mapper;

import cn.lili.modules.distribution.entity.dos.Distribution;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Update;


/**
 * 分销员数据处理层
 *
 * @author pikachu
 * @date 2020-03-14 23:04:56
 */
public interface DistributionMapper extends BaseMapper<Distribution> {

    @Update("UPDATE li_distribution set can_rebate = can_rebate+#{canRebate} WHERE id = #{distributionId}")
    void subCanRebate(Double canRebate,String distributionId);

    @Update("UPDATE li_distribution set can_rebate = (can_rebate+#{canRebate}) AND rebate_total=(rebate_total+#{canRebate}) AND distribution_order_count=(distribution_order_count+1) WHERE id = #{distributionId}")
    void addCanRebate(Double canRebate,String distributionId);
}