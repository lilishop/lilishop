package cn.lili.modules.distribution.mapper;

import cn.lili.modules.distribution.entity.dos.Distribution;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;

import java.math.BigDecimal;

/**
 * 分销员数据处理层
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
public interface DistributionMapper extends BaseMapper<Distribution> {

    /**
     * 按奖励类型汇总分销员佣金（扣除退款冲回）。
     *
     * @param distributionId 分销员 ID
     * @param rewardType       DIRECT 一级佣金，INVITE 二级邀请佣金
     */
    @Select("<script>"
            + "SELECT COALESCE(SUM(GREATEST("
            + "<choose>"
            + "<when test=\"rewardType == 'DIRECT'\">IFNULL(do.rebate, 0) - IFNULL(do.sell_back_rebate, 0)</when>"
            + "<otherwise>IFNULL(do.invite_commission, 0) - IFNULL(do.invite_sell_back_rebate, 0)</otherwise>"
            + "</choose>"
            + ", 0)), 0) FROM li_distribution_order do "
            + "WHERE do.distribution_order_status &lt;&gt; 'REFUND' "
            + "<choose>"
            + "<when test=\"rewardType == 'DIRECT'\">AND do.distribution_id = #{distributionId}</when>"
            + "<otherwise>AND do.invite_distribution_id = #{distributionId}</otherwise>"
            + "</choose>"
            + "</script>")
    BigDecimal sumRewardByType(@Param("distributionId") String distributionId,
                               @Param("rewardType") String rewardType);

    /**
     * 汇总分销员推广/自购销售额。
     *
     * @param distributionId 分销员 ID
     * @param selfPurchase   是否自购：0 推广，1 自购
     */
    @Select("SELECT COALESCE(SUM(oi.flow_price), 0) FROM li_distribution_order do "
            + "INNER JOIN li_order_item oi ON oi.sn = do.order_item_sn AND oi.delete_flag = 0 "
            + "WHERE do.distribution_id = #{distributionId} "
            + "AND IFNULL(do.self_purchase, 0) = #{selfPurchase}")
    BigDecimal sumOrderSales(@Param("distributionId") String distributionId,
                             @Param("selfPurchase") int selfPurchase);

    /**
     * 统计推广订单数（按主订单号去重，不含自购）。
     */
    @Select("SELECT COUNT(DISTINCT do.order_sn) FROM li_distribution_order do "
            + "WHERE do.distribution_id = #{distributionId} AND IFNULL(do.self_purchase, 0) = 0")
    Long countPromotionOrders(@Param("distributionId") String distributionId);

    /**
     * 汇总绑定客户首单金额。
     */
    @Select("SELECT COALESCE(SUM(o.flow_price), 0) FROM li_distribution_customer_relation cr "
            + "INNER JOIN li_order o ON o.sn = ("
            + "  SELECT do2.order_sn FROM li_distribution_order do2 "
            + "  WHERE do2.distribution_id = #{distributionId} "
            + "    AND do2.member_id = cr.member_id "
            + "    AND IFNULL(do2.self_purchase, 0) = 0 "
            + "  ORDER BY IFNULL(do2.payment_time, do2.create_time) ASC, do2.create_time ASC "
            + "  LIMIT 1"
            + ") AND IFNULL(o.delete_flag, 0) = 0 "
            + "WHERE cr.distribution_id = #{distributionId} AND IFNULL(cr.delete_flag, 0) = 0")
    BigDecimal sumFirstOrderAmount(@Param("distributionId") String distributionId);

    /**
     * 扣减冻结佣金与推广订单统计（订单取消等场景）。
     */
    @Update("UPDATE li_distribution SET commission_frozen = (IFNULL(commission_frozen, 0) - #{commissionFrozen}), "
            + "rebate_total = (IFNULL(rebate_total, 0) - #{commissionFrozen}), "
            + "distribution_order_price = (IFNULL(distribution_order_price, 0) - #{distributionOrderPrice}), "
            + "distribution_order_count = (IFNULL(distribution_order_count, 0) - 1) "
            + "WHERE id = #{distributionId}")
    void subRebate(Double commissionFrozen, String distributionId, Double distributionOrderPrice);

    /**
     * 增加冻结佣金与推广订单统计（下单冻结场景）。
     */
    @Update("UPDATE li_distribution SET commission_frozen = (IFNULL(commission_frozen, 0) + #{commissionFrozen}), "
            + "rebate_total = (IFNULL(rebate_total, 0) + #{commissionFrozen}), "
            + "distribution_order_price = (IFNULL(distribution_order_price, 0) + #{distributionOrderPrice}), "
            + "distribution_order_count = (IFNULL(distribution_order_count, 0) + 1) "
            + "WHERE id = #{distributionId}")
    void addRebate(Double commissionFrozen, String distributionId, Double distributionOrderPrice);

    /**
     * 冻结佣金转可提现余额。
     */
    @Update("UPDATE li_distribution SET commission_frozen = (IFNULL(commission_frozen, 0) - #{rebate}), "
            + "can_rebate = (IFNULL(can_rebate, 0) + #{rebate}) "
            + "WHERE id = #{distributionId}")
    void addCanRebate(Double rebate, String distributionId);

    /**
     * 可提现余额转已提现金额。
     */
    @Update("UPDATE li_distribution SET can_rebate = (IFNULL(can_rebate, 0) - #{rebate}), "
            + "cash_rebate = (IFNULL(cash_rebate, 0) + #{rebate}) "
            + "WHERE id = #{distributionId}")
    void addCashRebate(Double rebate, String distributionId);

    /**
     * 扣减已提现金额（提现拒绝回退等场景）。
     */
    @Update("UPDATE li_distribution SET cash_rebate = (IFNULL(cash_rebate, 0) - #{rebate}) "
            + "WHERE id = #{distributionId}")
    void subCashRebate(Double rebate, String distributionId);

    /**
     * 按邀请关系重算累计邀请下级分销员数。
     * 内层派生表避免 MySQL 同表 UPDATE + SELECT 报错。
     */
    @Update("UPDATE li_distribution SET invite_count = ("
            + "SELECT cnt FROM ("
            + "SELECT COUNT(1) AS cnt FROM li_distribution child "
            + "WHERE child.parent_distribution_id = #{distributionId} AND IFNULL(child.delete_flag, 0) = 0"
            + ") AS invite_cnt"
            + ") WHERE id = #{distributionId}")
    void syncInviteCount(@Param("distributionId") String distributionId);

    /**
     * 按有效客户关系重算累计有效客户数（不含自绑定、已失效）。
     */
    @Update("UPDATE li_distribution SET valid_customer_count = ("
            + "SELECT COUNT(1) FROM li_distribution_customer_relation r "
            + "WHERE r.distribution_id = #{distributionId} AND IFNULL(r.delete_flag, 0) = 0 "
            + "AND r.relation_status IN ('PENDING', 'TEMPORARY', 'PERMANENT')"
            + ") WHERE id = #{distributionId}")
    void syncValidCustomerCount(@Param("distributionId") String distributionId);
}
