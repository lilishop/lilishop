package cn.lili.modules.finance.serviceimpl;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.finance.entity.dto.FinanceReportSearchParams;
import cn.lili.modules.finance.entity.vo.*;
import cn.lili.modules.finance.export.FinanceExportHelper;
import cn.lili.modules.finance.mapper.FinanceReportMapper;
import cn.lili.modules.finance.service.FinanceReportService;
import cn.hutool.poi.excel.ExcelWriter;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 财务报表服务实现。
 * <p>
 * 负责解析查询时间范围、调用 Mapper 聚合查询，并将结果写出为 Excel。
 * 未传时间时默认查询近 30 天。
 */
@Service
public class FinanceReportServiceImpl implements FinanceReportService {

    @Autowired
    private FinanceReportMapper financeReportMapper;

    @Override
    public List<PlatformPeriodReportVO> platformPeriodReport(FinanceReportSearchParams params) {
        DateRange range = resolveRange(params);
        String dateFormat = FinanceReportSearchParams.GRANULARITY_MONTH.equals(params.getGranularity())
                ? "%Y-%m" : "%Y-%m-%d";
        return financeReportMapper.platformPeriodReport(range.start(), range.end(), params.getStoreId(), dateFormat);
    }

    @Override
    public List<StoreSettlementSummaryVO> storeSettlementSummary(FinanceReportSearchParams params) {
        DateRange range = resolveRange(params);
        return financeReportMapper.storeSettlementSummary(range.start(), range.end(), params.getStoreId());
    }

    @Override
    public List<PaymentMethodSummaryVO> paymentMethodSummary(FinanceReportSearchParams params) {
        DateRange range = resolveRange(params);
        return financeReportMapper.paymentMethodSummary(range.start(), range.end(), params.getStoreId());
    }

    @Override
    public List<SettlementLedgerVO> settlementLedger(FinanceReportSearchParams params) {
        return financeReportMapper.settlementLedger(params.getStoreId());
    }

    @Override
    public StorePeriodSummaryVO storePeriodSummary(String storeId, FinanceReportSearchParams params) {
        DateRange range = resolveRange(params);
        StorePeriodSummaryVO vo = financeReportMapper.storePeriodSummary(storeId, range.start(), range.end());
        if (vo == null) {
            vo = new StorePeriodSummaryVO();
        }
        return vo;
    }

    @Override
    public void exportPlatformPeriodReport(HttpServletResponse response, FinanceReportSearchParams params) {
        List<PlatformPeriodReportVO> data = platformPeriodReport(params);
        FinanceExportHelper.checkRowLimit(data.size());
        FinanceExportHelper.writeExcel(response, "平台经营报表", writer -> {
            writer.setSheet("平台经营报表");
            writer.writeHeadRow(List.of("周期", "GMV", "退款", "净GMV", "佣金收入", "退佣返还",
                    "券补贴", "退券补贴", "礼品卡补贴", "退礼品卡补贴", "积分补贴", "退积分补贴",
                    "砍价补贴", "退砍价补贴", "直降补贴", "退直降补贴",
                    "第N件补贴", "退第N件补贴", "分销支出", "退分销", "商家应结"));
            writeRows(writer, data.stream().map(this::platformRow).toList());
        });
    }

    @Override
    public void exportStoreSettlementSummary(HttpServletResponse response, FinanceReportSearchParams params) {
        List<StoreSettlementSummaryVO> data = storeSettlementSummary(params);
        FinanceExportHelper.checkRowLimit(data.size());
        FinanceExportHelper.writeExcel(response, "店铺结算汇总", writer -> {
            writer.setSheet("店铺结算汇总");
            writer.writeHeadRow(List.of("店铺", "订单实付", "退款", "佣金", "分销", "券补贴",
                    "礼品卡补贴", "应结", "已出账", "已对账", "已付款"));
            writeRows(writer, data.stream().map(this::storeRow).toList());
        });
    }

    @Override
    public void exportPaymentMethodSummary(HttpServletResponse response, FinanceReportSearchParams params) {
        List<PaymentMethodSummaryVO> data = paymentMethodSummary(params);
        FinanceExportHelper.checkRowLimit(data.size());
        FinanceExportHelper.writeExcel(response, "支付方式汇总", writer -> {
            writer.setSheet("支付方式汇总");
            writer.writeHeadRow(List.of("日期", "支付方式", "支付笔数", "支付金额", "退款笔数", "退款金额"));
            writeRows(writer, data.stream().map(row -> List.<Object>of(
                    row.getPeriod(), row.getPaymentName(), row.getPayCount(),
                    row.getPayAmount(), row.getRefundCount(), row.getRefundAmount())).toList());
        });
    }

    @Override
    public void exportSettlementLedger(HttpServletResponse response, FinanceReportSearchParams params) {
        List<SettlementLedgerVO> data = settlementLedger(params);
        FinanceExportHelper.checkRowLimit(data.size());
        FinanceExportHelper.writeExcel(response, "结算台账", writer -> {
            writer.setSheet("结算台账");
            writer.writeHeadRow(List.of("店铺", "待结算流水", "已出账未付", "已对账未付", "已付款累计"));
            writeRows(writer, data.stream().map(row -> List.<Object>of(
                    row.getStoreName(), row.getPendingFlowAmount(), row.getOutUnpaidAmount(),
                    row.getCheckUnpaidAmount(), row.getPaidAmount())).toList());
        });
    }

    @Override
    public void exportStorePeriodSummary(HttpServletResponse response, String storeId, FinanceReportSearchParams params) {
        StorePeriodSummaryVO vo = storePeriodSummary(storeId, params);
        FinanceExportHelper.writeExcel(response, "店铺周期汇总", writer -> {
            writer.setSheet("店铺周期汇总");
            writer.writeHeadRow(List.of("店铺", "订单实付", "退款", "平台服务费", "分销佣金", "券补贴", "礼品卡补贴", "应结"));
            writer.writeRow(List.of(vo.getStoreName(), vo.getOrderPrice(), vo.getRefundPrice(),
                    vo.getCommissionPrice(), vo.getDistributionCommission(), vo.getSiteCouponCommission(),
                    vo.getGiftCardSubsidy(), vo.getBillPrice()), false);
        });
    }

    private List<Object> platformRow(PlatformPeriodReportVO row) {
        return List.of(row.getPeriod(), row.getGmv(), row.getRefundAmount(), row.getNetGmv(),
                row.getCommissionIncome(), row.getRefundCommission(), row.getCouponSubsidy(),
                row.getRefundCouponSubsidy(), row.getGiftCardSubsidy(), row.getRefundGiftCardSubsidy(),
                row.getPointSubsidy(), row.getRefundPointSubsidy(), row.getKanjiaSubsidy(),
                row.getRefundKanjiaSubsidy(), row.getFlashDiscountSubsidy(), row.getRefundFlashDiscountSubsidy(),
                row.getNthItemSubsidy(), row.getRefundNthItemSubsidy(), row.getDistributionExpense(), row.getRefundDistribution(),
                row.getStoreSettlementTotal());
    }

    private List<Object> storeRow(StoreSettlementSummaryVO row) {
        return List.of(row.getStoreName(), row.getOrderPrice(), row.getRefundPrice(), row.getCommissionPrice(),
                row.getDistributionCommission(), row.getSiteCouponCommission(), row.getGiftCardSubsidy(),
                row.getBillPrice(), row.getOutBillAmount(), row.getCheckBillAmount(), row.getCompleteBillAmount());
    }

    private void writeRows(ExcelWriter writer, List<List<Object>> rows) {
        if (rows != null && !rows.isEmpty()) {
            writer.write(rows, false);
        }
    }

    /**
     * 解析查询时间范围。
     * 优先级：startTime/endTime &gt; startDate/endDate &gt; 默认近 30 天。
     */
    private DateRange resolveRange(FinanceReportSearchParams params) {
        Date start;
        Date end;
        if (params.getStartTime() != null && params.getEndTime() != null) {
            start = params.getStartTime();
            end = params.getEndTime();
        } else if (CharSequenceUtil.isNotEmpty(params.getStartDate()) && CharSequenceUtil.isNotEmpty(params.getEndDate())) {
            start = DateUtil.parse(params.getStartDate());
            end = DateUtil.endOfDay(DateUtil.parse(params.getEndDate()));
        } else {
            end = new Date();
            start = DateUtil.offsetDay(end, -30);
        }
        if (!start.before(end)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "开始时间必须早于结束时间");
        }
        return new DateRange(start, end);
    }

    /** 报表查询闭开区间：[start, end) */
    private record DateRange(Date start, Date end) {
    }
}
