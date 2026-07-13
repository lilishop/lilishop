package cn.lili.controller.manager.finance;

import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.finance.entity.dto.FinanceReportSearchParams;
import cn.lili.modules.finance.entity.vo.*;
import cn.lili.modules.finance.service.FinanceExportService;
import cn.lili.modules.finance.service.FinanceReportService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.store.entity.dto.BillSearchParams;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 管理端财务中心 API。
 * <p>
 * 提供平台维度的流水导出、结算单批量下载，以及 4 张经营/结算报表的查询与导出。
 * 所有导出接口直接写出文件流（{@code application/octet-stream}），不包装为 JSON。
 */
@RestController
@Tag(name = "管理端,财务中心")
@RequestMapping("/manager/finance")
public class FinanceManagerController {

    @Autowired
    private FinanceExportService financeExportService;

    @Autowired
    private FinanceReportService financeReportService;

    @Operation(summary = "导出支付流水")
    @GetMapping(value = "/payment-flow/export", produces = "application/octet-stream")
    public void exportPaymentFlow(Order order, SearchVO searchVO) {
        financeExportService.exportPaymentFlow(ThreadContextHolder.getHttpResponse(), order, searchVO);
    }

    @Operation(summary = "导出退款流水")
    @GetMapping(value = "/refund-flow/export", produces = "application/octet-stream")
    public void exportRefundFlow(RefundLog entity, SearchVO searchVO) {
        financeExportService.exportRefundFlow(ThreadContextHolder.getHttpResponse(), entity, searchVO);
    }

    @Operation(summary = "导出钱包流水")
    @GetMapping(value = "/wallet-log/export", produces = "application/octet-stream")
    public void exportWalletLog(@RequestParam(required = false) String memberId,
                                @RequestParam(required = false) String memberName,
                                @RequestParam(required = false) String startDate,
                                @RequestParam(required = false) String endDate) {
        financeExportService.exportWalletLog(ThreadContextHolder.getHttpResponse(),
                memberId, memberName, startDate, endDate);
    }

    @Operation(summary = "导出结算单列表")
    @GetMapping(value = "/bill-list/export", produces = "application/octet-stream")
    public void exportBillList(BillSearchParams params) {
        financeExportService.exportBillList(ThreadContextHolder.getHttpResponse(), params);
    }

    @Operation(summary = "批量下载结算单")
    @PostMapping(value = "/bill/batchDownload", produces = "application/octet-stream")
    public void batchDownloadBills(@RequestBody List<String> billIds) {
        financeExportService.batchDownloadBills(ThreadContextHolder.getHttpResponse(), billIds);
    }

    @Operation(summary = "平台经营报表")
    @GetMapping("/report/platform")
    public ResultMessage<List<PlatformPeriodReportVO>> platformReport(FinanceReportSearchParams params) {
        return ResultUtil.data(financeReportService.platformPeriodReport(params));
    }

    @Operation(summary = "导出平台经营报表")
    @GetMapping(value = "/report/platform/export", produces = "application/octet-stream")
    public void exportPlatformReport(FinanceReportSearchParams params) {
        financeReportService.exportPlatformPeriodReport(ThreadContextHolder.getHttpResponse(), params);
    }

    @Operation(summary = "店铺结算汇总")
    @GetMapping("/report/store-settlement")
    public ResultMessage<List<StoreSettlementSummaryVO>> storeSettlement(FinanceReportSearchParams params) {
        return ResultUtil.data(financeReportService.storeSettlementSummary(params));
    }

    @Operation(summary = "导出店铺结算汇总")
    @GetMapping(value = "/report/store-settlement/export", produces = "application/octet-stream")
    public void exportStoreSettlement(FinanceReportSearchParams params) {
        financeReportService.exportStoreSettlementSummary(ThreadContextHolder.getHttpResponse(), params);
    }

    @Operation(summary = "支付方式汇总")
    @GetMapping("/report/payment-method")
    public ResultMessage<List<PaymentMethodSummaryVO>> paymentMethod(FinanceReportSearchParams params) {
        return ResultUtil.data(financeReportService.paymentMethodSummary(params));
    }

    @Operation(summary = "导出支付方式汇总")
    @GetMapping(value = "/report/payment-method/export", produces = "application/octet-stream")
    public void exportPaymentMethod(FinanceReportSearchParams params) {
        financeReportService.exportPaymentMethodSummary(ThreadContextHolder.getHttpResponse(), params);
    }

    @Operation(summary = "结算台账")
    @GetMapping("/report/settlement-ledger")
    public ResultMessage<List<SettlementLedgerVO>> settlementLedger(FinanceReportSearchParams params) {
        return ResultUtil.data(financeReportService.settlementLedger(params));
    }

    @Operation(summary = "导出结算台账")
    @GetMapping(value = "/report/settlement-ledger/export", produces = "application/octet-stream")
    public void exportSettlementLedger(FinanceReportSearchParams params) {
        financeReportService.exportSettlementLedger(ThreadContextHolder.getHttpResponse(), params);
    }
}
