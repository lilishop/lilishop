package cn.lili.modules.statistics.mapper;

import cn.lili.modules.statistics.entity.vo.PointsSourceDistributionVO;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Select;

import java.util.List;
import java.util.Map;

/**
 * 积分分析统计数据处理层
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Mapper
public interface PointsStatisticsMapper {

    @Select("SELECT " +
            "CASE " +
            " WHEN point >= 1 AND point < 2000 THEN '1-2000' " +
            " WHEN point >= 2000 AND point < 10000 THEN '2000-10000' " +
            " WHEN point >= 10000 AND point < 20000 THEN '10000-20000' " +
            " WHEN point >= 20000 AND point < 50000 THEN '20000-50000' " +
            " WHEN point >= 50000 THEN '50000-更多' " +
            "END AS pointRange, COUNT(0) AS memberNum " +
            "FROM li_member WHERE disabled = 1 AND point >= 1 " +
            "GROUP BY pointRange")
    List<Map<String, Object>> pointBucketDistribution();

    @Select("SELECT IFNULL(point_source,'UNKNOWN') AS source, COALESCE(SUM(variable_point),0) AS point " +
            "FROM li_member_points_history WHERE point_type = 'INCREASE' GROUP BY IFNULL(point_source,'UNKNOWN')")
    List<PointsSourceDistributionVO> sourceDistribution();

    @Select("SELECT IFNULL(g.grade_name,'默认等级/未分组') AS identity, " +
            "COALESCE(SUM(m.total_point),0) AS totalIssued, " +
            "COALESCE(SUM(m.point),0) AS available, " +
            "COUNT(0) AS memberCount " +
            "FROM li_member m LEFT JOIN li_member_grade g ON m.grade_id = g.id " +
            "WHERE m.disabled = 1 GROUP BY m.grade_id, g.grade_name")
    List<Map<String, Object>> identityStat();
}
