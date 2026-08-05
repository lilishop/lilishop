package cn.lili.modules.distribution.mapper;

import cn.lili.modules.distribution.entity.vos.DistributionInviteeVO;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * 分销员邀请下级查询
 *
 * @author Bulbasaur
 * @since 2026-08-05
 */
public interface DistributionInviteMapper {

    /** 下级分销员筛选条件（仅计数） */
    String INVITEE_FILTER_SQL =
            "FROM li_distribution child "
                    + "WHERE child.parent_distribution_id = #{parentDistributionId} AND child.delete_flag = 0 "
                    + "<if test='startTime != null'> AND child.join_time &gt;= #{startTime} </if>"
                    + "<if test='endTime != null'> AND child.join_time &lt;= #{endTime} </if>";

    /** 下级分销员列表关联查询（含业绩与邀请佣金） */
    String INVITEE_JOIN_SQL =
            "FROM li_distribution child "
                    + "INNER JOIN li_distribution d ON d.id = child.id AND d.delete_flag = 0 "
                    + "LEFT JOIN li_member m ON m.id = d.member_id AND m.delete_flag = 0 "
                    + "LEFT JOIN li_distribution_grade dg ON dg.grade_sort = d.grade_id AND dg.delete_flag = 0 "
                    + "LEFT JOIN li_distribution_order do ON do.distribution_id = child.id "
                    + "AND do.invite_distribution_id = child.parent_distribution_id "
                    + "AND IFNULL(do.distribution_order_status, '') &lt;&gt; 'REFUND' "
                    + "WHERE child.parent_distribution_id = #{parentDistributionId} AND child.delete_flag = 0 "
                    + "<if test='startTime != null'> AND child.join_time &gt;= #{startTime} </if>"
                    + "<if test='endTime != null'> AND child.join_time &lt;= #{endTime} </if>";

    /**
     * 统计邀请下级数量。
     */
    @Select("<script>SELECT COUNT(1) " + INVITEE_FILTER_SQL + "</script>")
    long countInvitees(@Param("parentDistributionId") String parentDistributionId,
                       @Param("startTime") Date startTime,
                       @Param("endTime") Date endTime);

    /**
     * 汇总邀请奖励金额。
     */
    @Select("<script>SELECT COALESCE(SUM(GREATEST(IFNULL(do.invite_commission, 0) - IFNULL(do.invite_sell_back_rebate, 0), 0)), 0) "
            + INVITEE_JOIN_SQL + "</script>")
    BigDecimal sumInviteReward(@Param("parentDistributionId") String parentDistributionId,
                               @Param("startTime") Date startTime,
                               @Param("endTime") Date endTime);

    /**
     * 分页查询邀请下级列表。
     */
    @Select("<script>SELECT child.id AS childDistributionId, "
            + "COALESCE(NULLIF(m.nick_name, ''), d.member_name) AS memberName, "
            + "m.face AS memberAvatar, "
            + "dg.grade_name AS gradeName, "
            + "child.join_time AS effectiveTime, "
            + "COALESCE(d.valid_sales_amount, d.distribution_order_price, 0) AS salesAmount, "
            + "COALESCE(d.valid_customer_count, 0) AS customerCount, "
            + "COALESCE(d.distribution_order_count, 0) AS orderCount, "
            + "COALESCE(SUM(GREATEST(IFNULL(do.invite_commission, 0) - IFNULL(do.invite_sell_back_rebate, 0), 0)), 0) AS inviteRewardAmount "
            + INVITEE_JOIN_SQL
            + "GROUP BY child.id, m.nick_name, d.member_name, m.face, dg.grade_name, child.join_time, "
            + "d.valid_sales_amount, d.distribution_order_price, d.valid_customer_count, d.distribution_order_count "
            + "ORDER BY child.join_time DESC "
            + "LIMIT #{offset}, #{pageSize}</script>")
    List<DistributionInviteeVO> listInvitees(@Param("parentDistributionId") String parentDistributionId,
                                             @Param("startTime") Date startTime,
                                             @Param("endTime") Date endTime,
                                             @Param("offset") long offset,
                                             @Param("pageSize") long pageSize);
}
