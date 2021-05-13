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
    void updateCanRebate(Double canRebate,String distributionId);
}