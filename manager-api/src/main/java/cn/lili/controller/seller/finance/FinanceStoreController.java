package cn.lili.controller.seller.finance;

import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.finance.entity.dto.FinanceReportSearchParams;
import cn.lili.modules.finance.entity.dto.FinanceStoreFlowSearchParams;
import cn.lili.modules.finance.entity.vo.StorePeriodSummaryVO;
import cn.lili.modules.finance.service.FinanceExportService;
import cn.lili.modules.finance.service.FinanceReportService;
import cn.lili.modules.store.entity.dto.BillSearchParams;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

/**
 * 商家端财务中心 API。
 * <p>
 * 所有接口均通过 {@link #currentStoreId()} 强制限定为当前登录店铺数据，
 * 忽略客户端传入的 storeId，防止越权访问他店流水与结算单。
 */
@RestController
@Tag(name = "店铺端,财务中心")
@RequestMapping("/store/finance")
public class FinanceStoreController {

    @Autowired
    private FinanceExportService financeExportService;

    @Autowired
    private FinanceReportService financeReportService;

    @Operation(summary = "导出本店流水")
    @GetMapping(value = "/store-flow/export", produces = "application/octet-stream")
    public void exportStoreFlow(FinanceStoreFlowSearchParams params) {
        params.setStoreId(currentStoreId());
        financeExportService.exportStoreFlow(ThreadContextHolder.getHttpResponse(), params);
    }

    @Operation(summary = "导出本店结算单列表")
    @GetMapping(value = "/bill-list/export", produces = "application/octet-stream")
    public void exportBillList(BillSearchParams params) {
        params.setStoreId(currentStoreId());
        financeExportService.exportBillList(ThreadContextHolder.getHttpResponse(), params);
    }

    @Operation(summary = "本店周期财务汇总")
    @GetMapping("/report/store-summary")
    public ResultMessage<StorePeriodSummaryVO> storeSummary(FinanceReportSearchParams params) {
        return ResultUtil.data(financeReportService.storePeriodSummary(currentStoreId(), params));
    }

    @Operation(summary = "导出本店周期财务汇总")
    @GetMapping(value = "/report/store-summary/export", produces = "application/octet-stream")
    public void exportStoreSummary(FinanceReportSearchParams params) {
        financeReportService.exportStorePeriodSummary(ThreadContextHolder.getHttpResponse(),
                currentStoreId(), params);
    }

    /**
     * 获取当前登录商家所属店铺 ID。
     *
     * @return 店铺 ID，未登录时抛出 NPE
     */
    private String currentStoreId() {
        return Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
    }
}
