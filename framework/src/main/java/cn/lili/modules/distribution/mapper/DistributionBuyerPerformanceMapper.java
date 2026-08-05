package cn.lili.modules.distribution.mapper;

import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 分销员端业绩统计查询
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionBuyerPerformanceMapper {

    /** 一级推广佣金（扣除退款冲回） */
    String DIRECT_COMMISSION_EXPR =
            "CASE WHEN do.distribution_order_status = 'REFUND' THEN 0 "
                    + "ELSE GREATEST(IFNULL(do.rebate, 0) - IFNULL(do.sell_back_rebate, 0), 0) END";

    /** 二级邀请佣金（扣除退款冲回） */
    String INVITE_COMMISSION_EXPR =
            "CASE WHEN do.distribution_order_status = 'REFUND' THEN 0 "
                    + "ELSE GREATEST(IFNULL(do.invite_commission, 0) - IFNULL(do.invite_sell_back_rebate, 0), 0) END";

    /** 一级待结算佣金 */
    String DIRECT_PENDING_EXPR =
            "CASE WHEN do.distribution_order_status = 'NO_COMPLETED' THEN (" + DIRECT_COMMISSION_EXPR + ") ELSE 0 END";

    /** 一级已结算佣金 */
    String DIRECT_SETTLED_EXPR =
            "CASE WHEN do.distribution_order_status = 'COMPLETE' THEN (" + DIRECT_COMMISSION_EXPR + ") ELSE 0 END";

    /** 二级待结算邀请佣金 */
    String INVITE_PENDING_EXPR =
            "CASE WHEN do.distribution_order_status = 'NO_COMPLETED' THEN (" + INVITE_COMMISSION_EXPR + ") ELSE 0 END";

    /** 二级已结算邀请佣金 */
    String INVITE_SETTLED_EXPR =
            "CASE WHEN do.distribution_order_status = 'COMPLETE' THEN (" + INVITE_COMMISSION_EXPR + ") ELSE 0 END";

    /** 下单时间范围筛选 */
    String TIME_FILTER =
            "<if test='startTime != null'> AND do.create_time &gt;= #{startTime} </if>"
                    + "<if test='endTime != null'> AND do.create_time &lt;= #{endTime} </if>";

    /** 推广订单统计 FROM 子句 */
    String PROMOTION_FROM =
            "FROM li_distribution_order do "
                    + "WHERE do.distribution_id = #{distributionId} " + TIME_FILTER;

    /** 邀请订单统计 FROM 子句 */
    String INVITE_FROM =
            "FROM li_distribution_order do "
                    + "WHERE do.invite_distribution_id = #{distributionId} " + TIME_FILTER;

    /**
     * 汇总一级推广佣金（含待结算与已结算）。
     */
    @Select("<script>SELECT COALESCE(SUM(" + DIRECT_COMMISSION_EXPR + "), 0) " + PROMOTION_FROM + "</script>")
    BigDecimal sumDirectCommission(@Param("distributionId") String distributionId,
                                   @Param("startTime") Date startTime,
                                   @Param("endTime") Date endTime);

    /**
     * 汇总一级待结算佣金。
     */
    @Select("<script>SELECT COALESCE(SUM(" + DIRECT_PENDING_EXPR + "), 0) " + PROMOTION_FROM + "</script>")
    BigDecimal sumDirectPending(@Param("distributionId") String distributionId,
                                @Param("startTime") Date startTime,
                                @Param("endTime") Date endTime);

    /**
     * 汇总一级已结算佣金。
     */
    @Select("<script>SELECT COALESCE(SUM(" + DIRECT_SETTLED_EXPR + "), 0) " + PROMOTION_FROM + "</script>")
    BigDecimal sumDirectSettled(@Param("distributionId") String distributionId,
                                @Param("startTime") Date startTime,
                                @Param("endTime") Date endTime);

    /**
     * 汇总二级邀请佣金（含待结算与已结算）。
     */
    @Select("<script>SELECT COALESCE(SUM(" + INVITE_COMMISSION_EXPR + "), 0) " + INVITE_FROM + "</script>")
    BigDecimal sumInviteCommission(@Param("distributionId") String distributionId,
                                   @Param("startTime") Date startTime,
                                   @Param("endTime") Date endTime);

    /**
     * 汇总二级待结算邀请佣金。
     */
    @Select("<script>SELECT COALESCE(SUM(" + INVITE_PENDING_EXPR + "), 0) " + INVITE_FROM + "</script>")
    BigDecimal sumInvitePending(@Param("distributionId") String distributionId,
                                @Param("startTime") Date startTime,
                                @Param("endTime") Date endTime);

    /**
     * 汇总二级已结算邀请佣金。
     */
    @Select("<script>SELECT COALESCE(SUM(" + INVITE_SETTLED_EXPR + "), 0) " + INVITE_FROM + "</script>")
    BigDecimal sumInviteSettled(@Param("distributionId") String distributionId,
                                @Param("startTime") Date startTime,
                                @Param("endTime") Date endTime);

    /**
     * 汇总推广销售额（非退款订单实付金额）。
     */
    @Select("<script>SELECT COALESCE(SUM(sf.final_price), 0) "
            + "FROM li_distribution_order do "
            + "LEFT JOIN li_store_flow sf ON sf.order_item_sn = do.order_item_sn AND sf.flow_type = 'PAY' "
            + "WHERE do.distribution_id = #{distributionId} "
            + "AND IFNULL(do.distribution_order_status, '') &lt;&gt; 'REFUND' " + TIME_FILTER + "</script>")
    BigDecimal sumPromotionSales(@Param("distributionId") String distributionId,
                                 @Param("startTime") Date startTime,
                                 @Param("endTime") Date endTime);
}
