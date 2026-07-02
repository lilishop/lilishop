package cn.lili.modules.finance.mapper;

import cn.lili.modules.finance.entity.vo.*;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.Date;
import java.util.List;

/**
 * 财务报表聚合查询 Mapper。
 * <p>
 * 所有报表均以 {@code li_store_flow} 为主表在 SQL 层聚合，
 * 商家应结金额使用 {@code bill_price} 字段（PAY 累加 − REFUND 累加），
 * 与 {@link cn.lili.modules.store.entity.dos.Bill#billPrice} 口径一致。
 */
public interface FinanceReportMapper {

    /**
     * 平台经营周期报表。
     * <p>
     * GMV = PAY 流水 final_price 合计；净 GMV = GMV − 退款；
     * 商家应结 = PAY bill_price 合计 − REFUND bill_price 合计。
     *
     * @param dateFormat MySQL DATE_FORMAT 模板，按日 {@code %Y-%m-%d} 或按月 {@code %Y-%m}
     */
    @Select("""
            SELECT DATE_FORMAT(sf.create_time, #{dateFormat}) AS period,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.final_price ELSE 0 END), 0) AS gmv,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.final_price ELSE 0 END), 0) AS refundAmount,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.final_price ELSE 0 END), 0)
                       - IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.final_price ELSE 0 END), 0) AS netGmv,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.commission_price ELSE 0 END), 0) AS commissionIncome,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.commission_price ELSE 0 END), 0) AS refundCommission,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.site_coupon_commission ELSE 0 END), 0) AS couponSubsidy,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.site_coupon_commission ELSE 0 END), 0) AS refundCouponSubsidy,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.gift_card_subsidy_price ELSE 0 END), 0) AS giftCardSubsidy,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.gift_card_subsidy_price ELSE 0 END), 0) AS refundGiftCardSubsidy,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.point_settlement_price ELSE 0 END), 0) AS pointSubsidy,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.point_settlement_price ELSE 0 END), 0) AS refundPointSubsidy,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.kanjia_settlement_price ELSE 0 END), 0) AS kanjiaSubsidy,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.kanjia_settlement_price ELSE 0 END), 0) AS refundKanjiaSubsidy,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.flash_discount_subsidy ELSE 0 END), 0) AS flashDiscountSubsidy,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.flash_discount_subsidy ELSE 0 END), 0) AS refundFlashDiscountSubsidy,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.nth_item_subsidy ELSE 0 END), 0) AS nthItemSubsidy,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.nth_item_subsidy ELSE 0 END), 0) AS refundNthItemSubsidy,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.distribution_rebate ELSE 0 END), 0) AS distributionExpense,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.distribution_rebate ELSE 0 END), 0) AS refundDistribution,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.bill_price ELSE 0 END), 0)
                       - IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.bill_price ELSE 0 END), 0) AS storeSettlementTotal
            FROM li_store_flow sf
            WHERE sf.create_time >= #{startTime}
              AND sf.create_time < #{endTime}
              AND (#{storeId} IS NULL OR #{storeId} = '' OR sf.store_id = #{storeId})
            GROUP BY period
            ORDER BY period
            """)
    List<PlatformPeriodReportVO> platformPeriodReport(@Param("startTime") Date startTime,
                                                      @Param("endTime") Date endTime,
                                                      @Param("storeId") String storeId,
                                                      @Param("dateFormat") String dateFormat);

    /**
     * 店铺结算汇总：流水维度聚合 + 关联 {@code li_bill} 各状态金额。
     * outBillAmount / checkBillAmount / completeBillAmount 分别对应 OUT / CHECK / COMPLETE 状态。
     */
    @Select("""
            SELECT sf.store_id AS storeId,
                   MAX(sf.store_name) AS storeName,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.final_price ELSE 0 END), 0) AS orderPrice,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.final_price ELSE 0 END), 0) AS refundPrice,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.commission_price ELSE 0 END), 0)
                       - IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.commission_price ELSE 0 END), 0) AS commissionPrice,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.distribution_rebate ELSE 0 END), 0)
                       - IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.distribution_rebate ELSE 0 END), 0) AS distributionCommission,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.site_coupon_commission ELSE 0 END), 0)
                       - IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.site_coupon_commission ELSE 0 END), 0) AS siteCouponCommission,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.gift_card_subsidy_price ELSE 0 END), 0)
                       - IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.gift_card_subsidy_price ELSE 0 END), 0) AS giftCardSubsidy,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.bill_price ELSE 0 END), 0)
                       - IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.bill_price ELSE 0 END), 0) AS billPrice,
                   IFNULL((SELECT SUM(b.bill_price) FROM li_bill b
                           WHERE b.store_id = sf.store_id AND b.bill_status = 'OUT'
                             AND b.create_time >= #{startTime} AND b.create_time < #{endTime}), 0) AS outBillAmount,
                   IFNULL((SELECT SUM(b.bill_price) FROM li_bill b
                           WHERE b.store_id = sf.store_id AND b.bill_status = 'CHECK'
                             AND b.create_time >= #{startTime} AND b.create_time < #{endTime}), 0) AS checkBillAmount,
                   IFNULL((SELECT SUM(b.bill_price) FROM li_bill b
                           WHERE b.store_id = sf.store_id AND b.bill_status = 'COMPLETE'
                             AND b.create_time >= #{startTime} AND b.create_time < #{endTime}), 0) AS completeBillAmount
            FROM li_store_flow sf
            WHERE sf.create_time >= #{startTime}
              AND sf.create_time < #{endTime}
              AND (#{storeId} IS NULL OR #{storeId} = '' OR sf.store_id = #{storeId})
            GROUP BY sf.store_id
            ORDER BY billPrice DESC
            """)
    List<StoreSettlementSummaryVO> storeSettlementSummary(@Param("startTime") Date startTime,
                                                          @Param("endTime") Date endTime,
                                                          @Param("storeId") String storeId);

    /** 按日期 × 支付方式汇总支付/退款笔数与金额 */
    @Select("""
            SELECT DATE_FORMAT(sf.create_time, '%Y-%m-%d') AS period,
                   IFNULL(sf.payment_name, 'UNKNOWN') AS paymentName,
                   SUM(CASE WHEN sf.flow_type = 'PAY' THEN 1 ELSE 0 END) AS payCount,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.final_price ELSE 0 END), 0) AS payAmount,
                   SUM(CASE WHEN sf.flow_type = 'REFUND' THEN 1 ELSE 0 END) AS refundCount,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.final_price ELSE 0 END), 0) AS refundAmount
            FROM li_store_flow sf
            WHERE sf.create_time >= #{startTime}
              AND sf.create_time < #{endTime}
              AND (#{storeId} IS NULL OR #{storeId} = '' OR sf.store_id = #{storeId})
            GROUP BY period, sf.payment_name
            ORDER BY period, paymentName
            """)
    List<PaymentMethodSummaryVO> paymentMethodSummary(@Param("startTime") Date startTime,
                                                      @Param("endTime") Date endTime,
                                                      @Param("storeId") String storeId);

    /**
     * 结算台账：全店铺维度。
     * pendingFlowAmount 取分账状态为 WAIT_COMPLETE 的流水应结合计；
     * 其余三项来自 {@code li_bill} 各状态累计。
     */
    @Select("""
            SELECT s.id AS storeId,
                   s.store_name AS storeName,
                   IFNULL((SELECT SUM(sf.bill_price) FROM li_store_flow sf
                           WHERE sf.store_id = s.id AND sf.profit_sharing_status = 'WAIT_COMPLETE'), 0) AS pendingFlowAmount,
                   IFNULL((SELECT SUM(b.bill_price) FROM li_bill b
                           WHERE b.store_id = s.id AND b.bill_status = 'OUT'), 0) AS outUnpaidAmount,
                   IFNULL((SELECT SUM(b.bill_price) FROM li_bill b
                           WHERE b.store_id = s.id AND b.bill_status = 'CHECK'), 0) AS checkUnpaidAmount,
                   IFNULL((SELECT SUM(b.bill_price) FROM li_bill b
                           WHERE b.store_id = s.id AND b.bill_status = 'COMPLETE'), 0) AS paidAmount
            FROM li_store s
            WHERE s.delete_flag = 0
              AND (#{storeId} IS NULL OR #{storeId} = '' OR s.id = #{storeId})
            ORDER BY pendingFlowAmount DESC
            """)
    List<SettlementLedgerVO> settlementLedger(@Param("storeId") String storeId);

    /**
     * 商家端单店周期汇总，字段与结算单汇总口径一致。
     */
    @Select("""
            SELECT MAX(sf.store_name) AS storeName,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.final_price ELSE 0 END), 0) AS orderPrice,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.final_price ELSE 0 END), 0) AS refundPrice,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.commission_price ELSE 0 END), 0)
                       - IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.commission_price ELSE 0 END), 0) AS commissionPrice,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.distribution_rebate ELSE 0 END), 0)
                       - IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.distribution_rebate ELSE 0 END), 0) AS distributionCommission,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.site_coupon_commission ELSE 0 END), 0)
                       - IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.site_coupon_commission ELSE 0 END), 0) AS siteCouponCommission,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.gift_card_subsidy_price ELSE 0 END), 0)
                       - IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.gift_card_subsidy_price ELSE 0 END), 0) AS giftCardSubsidy,
                   IFNULL(SUM(CASE WHEN sf.flow_type = 'PAY' THEN sf.bill_price ELSE 0 END), 0)
                       - IFNULL(SUM(CASE WHEN sf.flow_type = 'REFUND' THEN sf.bill_price ELSE 0 END), 0) AS billPrice
            FROM li_store_flow sf
            WHERE sf.store_id = #{storeId}
              AND sf.create_time >= #{startTime}
              AND sf.create_time < #{endTime}
            """)
    StorePeriodSummaryVO storePeriodSummary(@Param("storeId") String storeId,
                                            @Param("startTime") Date startTime,
                                            @Param("endTime") Date endTime);
}
