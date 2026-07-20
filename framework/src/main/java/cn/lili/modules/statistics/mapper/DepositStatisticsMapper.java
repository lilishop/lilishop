package cn.lili.modules.statistics.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Select;

import java.util.List;
import java.util.Map;

/**
 * 储值分析统计数据处理层
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Mapper
public interface DepositStatisticsMapper {

    @Select("SELECT " +
            "CASE " +
            " WHEN member_wallet >= 0 AND member_wallet < 30 THEN '0-30' " +
            " WHEN member_wallet >= 30 AND member_wallet < 60 THEN '30-60' " +
            " WHEN member_wallet >= 60 AND member_wallet < 90 THEN '60-90' " +
            " WHEN member_wallet >= 90 AND member_wallet < 120 THEN '90-120' " +
            " WHEN member_wallet >= 120 AND member_wallet < 150 THEN '120-150' " +
            " WHEN member_wallet >= 150 AND member_wallet < 180 THEN '150-180' " +
            " WHEN member_wallet >= 180 AND member_wallet < 210 THEN '180-210' " +
            " WHEN member_wallet >= 210 AND member_wallet < 700 THEN '210-700' " +
            " WHEN member_wallet >= 700 AND member_wallet < 800 THEN '700-800' " +
            " WHEN member_wallet >= 800 THEN '800-更多' " +
            "END AS label, COUNT(0) AS memberNum " +
            "FROM li_member_wallet WHERE member_wallet IS NOT NULL " +
            "GROUP BY label HAVING label IS NOT NULL")
    List<Map<String, Object>> balanceDistribution();

    @Select("SELECT " +
            "CASE " +
            " WHEN cnt >= 0 AND cnt <= 1 THEN '0-1' " +
            " WHEN cnt BETWEEN 2 AND 3 THEN '2-3' " +
            " WHEN cnt BETWEEN 4 AND 5 THEN '4-5' " +
            " WHEN cnt BETWEEN 6 AND 7 THEN '6-7' " +
            " WHEN cnt BETWEEN 8 AND 9 THEN '8-9' " +
            " WHEN cnt BETWEEN 10 AND 11 THEN '10-11' " +
            " WHEN cnt BETWEEN 12 AND 13 THEN '12-13' " +
            " WHEN cnt >= 14 THEN '14-更多' " +
            "END AS label, COUNT(0) AS memberNum FROM (" +
            " SELECT member_id, COUNT(0) AS cnt FROM li_recharge WHERE pay_status='PAID' GROUP BY member_id" +
            ") t GROUP BY label")
    List<Map<String, Object>> rechargeTimesDistribution();

    @Select("SELECT " +
            "CASE " +
            " WHEN amt >= 0 AND amt < 100 THEN '0-100' " +
            " WHEN amt >= 100 AND amt < 300 THEN '100-300' " +
            " WHEN amt >= 300 AND amt < 500 THEN '300-500' " +
            " WHEN amt >= 500 AND amt < 1000 THEN '500-1000' " +
            " WHEN amt >= 1000 AND amt < 2000 THEN '1000-2000' " +
            " WHEN amt >= 2000 AND amt < 5000 THEN '2000-5000' " +
            " WHEN amt >= 5000 AND amt < 10000 THEN '5000-10000' " +
            " WHEN amt >= 10000 THEN '10000-更多' " +
            "END AS label, COUNT(0) AS memberNum FROM (" +
            " SELECT member_id, COALESCE(SUM(recharge_money),0) AS amt FROM li_recharge WHERE pay_status='PAID' GROUP BY member_id" +
            ") t GROUP BY label")
    List<Map<String, Object>> rechargeAmountDistribution();
}
