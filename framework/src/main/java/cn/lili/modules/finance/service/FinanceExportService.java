package cn.lili.modules.finance.service;

import cn.lili.common.vo.SearchVO;
import cn.lili.modules.finance.entity.dto.FinanceStoreFlowSearchParams;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.store.entity.dto.BillSearchParams;
import jakarta.servlet.http.HttpServletResponse;

import java.util.List;

/**
 * 财务数据导出服务。
 * <p>
 * 统一承担结算单、支付/退款/钱包流水及店铺流水的 Excel 导出，
 * 避免在 {@code BillServiceImpl}、{@code OrderServiceImpl} 等处重复拼装 POI 逻辑。
 * 单次导出行数上限见 {@link cn.lili.modules.finance.export.FinanceExportHelper#MAX_EXPORT_ROWS}。
 */
public interface FinanceExportService {

    /**
     * 导出单张结算单（汇总 + 入账明细 + 退款明细，三 Sheet xlsx）。
     *
     * @param response HTTP 响应，直接写出文件流
     * @param billId   结算单 ID
     */
    void exportBill(HttpServletResponse response, String billId);

    /**
     * 按筛选条件导出结算单列表（仅汇总行，不含明细）。
     *
     * @param params 与 {@code BillService.billPage} 相同的查询参数
     */
    void exportBillList(HttpServletResponse response, BillSearchParams params);

    /**
     * 批量下载多张结算单，打包为 ZIP（每张单一个 xlsx）。
     *
     * @param billIds 结算单 ID 列表
     */
    void batchDownloadBills(HttpServletResponse response, List<String> billIds);

    /**
     * 导出支付流水，筛选条件与支付日志列表一致。
     *
     * @param order    订单筛选实体（支付方式、支付状态等）
     * @param searchVO 时间范围等通用搜索条件
     */
    void exportPaymentFlow(HttpServletResponse response, Order order, SearchVO searchVO);

    /**
     * 导出退款流水，筛选条件与退款日志列表一致。
     */
    void exportRefundFlow(HttpServletResponse response, RefundLog entity, SearchVO searchVO);

    /**
     * 导出会员钱包变动流水。
     *
     * @param memberId   会员 ID（可选）
     * @param memberName 会员名称模糊匹配（可选）
     * @param startDate  开始日期（可选）
     * @param endDate    结束日期（可选）
     */
    void exportWalletLog(HttpServletResponse response, String memberId, String memberName,
                         String startDate, String endDate);

    /**
     * 导出店铺流水明细（商家端强制传入 storeId）。
     */
    void exportStoreFlow(HttpServletResponse response, FinanceStoreFlowSearchParams params);
}
