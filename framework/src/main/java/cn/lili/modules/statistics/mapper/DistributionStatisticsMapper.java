package cn.lili.modules.statistics.mapper;

import cn.lili.modules.statistics.entity.vo.DistributionRankVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 分销统计数据处理层
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Mapper
public interface DistributionStatisticsMapper {

    @Select("SELECT COUNT(0) FROM li_distribution ${ew.customSqlSegment}")
    Long countDistributor(@Param(Constants.WRAPPER) Wrapper queryWrapper);

    @Select("SELECT COUNT(0) FROM li_distribution_order ${ew.customSqlSegment}")
    Long countDistributionOrder(@Param(Constants.WRAPPER) Wrapper queryWrapper);

    @Select("SELECT COALESCE(SUM(rebate),0) FROM li_distribution_order ${ew.customSqlSegment}")
    Double sumRebate(@Param(Constants.WRAPPER) Wrapper queryWrapper);

    @Select("SELECT COALESCE(SUM(price),0) FROM li_distribution_cash ${ew.customSqlSegment}")
    Double sumCash(@Param(Constants.WRAPPER) Wrapper queryWrapper);

    @Select("SELECT distribution_id AS id, MAX(distribution_name) AS name, COALESCE(SUM(rebate),0) AS amount, COUNT(0) AS num " +
            "FROM li_distribution_order ${ew.customSqlSegment}")
    List<DistributionRankVO> rankDistributor(@Param(Constants.WRAPPER) Wrapper queryWrapper);

    @Select("SELECT sku_id AS id, MAX(goods_name) AS name, COALESCE(SUM(rebate),0) AS amount, COALESCE(SUM(num),0) AS num " +
            "FROM li_distribution_order ${ew.customSqlSegment}")
    List<DistributionRankVO> rankGoods(@Param(Constants.WRAPPER) Wrapper queryWrapper);
}
