package cn.lili.modules.finance.service;

import cn.lili.modules.finance.entity.dto.FinanceReportSearchParams;
import cn.lili.modules.finance.entity.vo.*;
import jakarta.servlet.http.HttpServletResponse;

import java.util.List;

/**
 * 财务报表服务。
 * <p>
 * 报表数据以 {@code li_store_flow} 为主表在数据库层聚合，与结算单生成逻辑同源，
 * 避免在 Java 内存中二次汇总导致口径偏差。
 */
public interface FinanceReportService {

    /**
     * 平台经营日报/月报：按日或月汇总 GMV、退款、佣金、各类补贴及商家应结。
     */
    List<PlatformPeriodReportVO> platformPeriodReport(FinanceReportSearchParams params);

    /**
     * 店铺结算汇总：按店铺聚合流水，并关联 {@code li_bill} 各状态金额。
     */
    List<StoreSettlementSummaryVO> storeSettlementSummary(FinanceReportSearchParams params);

    /**
     * 支付方式汇总：按日期 × 支付方式统计笔数与金额。
     */
    List<PaymentMethodSummaryVO> paymentMethodSummary(FinanceReportSearchParams params);

    /**
     * 结算台账：各店铺待结算流水、已出账/已对账未付、已付款累计。
     */
    List<SettlementLedgerVO> settlementLedger(FinanceReportSearchParams params);

    /**
     * 商家端本店周期汇总（单店铺、指定时间范围内的流水合计）。
     *
     * @param storeId 当前登录店铺 ID，由 Controller 注入
     */
    StorePeriodSummaryVO storePeriodSummary(String storeId, FinanceReportSearchParams params);

    void exportPlatformPeriodReport(HttpServletResponse response, FinanceReportSearchParams params);

    void exportStoreSettlementSummary(HttpServletResponse response, FinanceReportSearchParams params);

    void exportPaymentMethodSummary(HttpServletResponse response, FinanceReportSearchParams params);

    void exportSettlementLedger(HttpServletResponse response, FinanceReportSearchParams params);

    void exportStorePeriodSummary(HttpServletResponse response, String storeId, FinanceReportSearchParams params);
}
