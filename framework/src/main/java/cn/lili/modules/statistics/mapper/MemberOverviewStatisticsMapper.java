package cn.lili.modules.statistics.mapper;

import cn.lili.modules.statistics.entity.vo.MemberGenderDistributionVO;
import cn.lili.modules.statistics.entity.vo.MemberRegionDistributionVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;
import java.util.Map;

/**
 * 会员概况统计数据处理层
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Mapper
public interface MemberOverviewStatisticsMapper {

    @Select("SELECT COUNT(0) FROM li_member ${ew.customSqlSegment}")
    Long countMember(@Param(Constants.WRAPPER) Wrapper queryWrapper);

    @Select("SELECT COUNT(DISTINCT member_id) FROM li_order ${ew.customSqlSegment}")
    Long countPayMember(@Param(Constants.WRAPPER) Wrapper queryWrapper);

    @Select("SELECT COUNT(0) FROM li_order ${ew.customSqlSegment}")
    Long countPayOrder(@Param(Constants.WRAPPER) Wrapper queryWrapper);

    @Select("SELECT COALESCE(SUM(flow_price),0) FROM li_order ${ew.customSqlSegment}")
    Double sumPayAmount(@Param(Constants.WRAPPER) Wrapper queryWrapper);

    @Select("SELECT COUNT(DISTINCT member_id) FROM li_recharge ${ew.customSqlSegment}")
    Long countRechargeMember(@Param(Constants.WRAPPER) Wrapper queryWrapper);

    @Select("SELECT sex, COUNT(0) AS num FROM li_member ${ew.customSqlSegment}")
    List<MemberGenderDistributionVO> genderDistribution(@Param(Constants.WRAPPER) Wrapper queryWrapper);

    @Select("SELECT IFNULL(region,'未知') AS region, COUNT(0) AS num FROM li_member ${ew.customSqlSegment}")
    List<MemberRegionDistributionVO> regionDistribution(@Param(Constants.WRAPPER) Wrapper queryWrapper);

    @Select("SELECT COUNT(DISTINCT member_id) AS pay_members, " +
            "COUNT(DISTINCT CASE WHEN cnt > 1 THEN member_id END) AS repurchase_members FROM (" +
            " SELECT member_id, COUNT(0) AS cnt FROM li_order ${ew.customSqlSegment} GROUP BY member_id) t")
    Map<String, Object> repurchaseStats(@Param(Constants.WRAPPER) Wrapper queryWrapper);
}
