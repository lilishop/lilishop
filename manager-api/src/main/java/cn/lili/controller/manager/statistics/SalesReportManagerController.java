package cn.lili.controller.manager.statistics;

import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.statistics.entity.dto.SalesReportQueryParam;
import cn.lili.modules.statistics.entity.vo.GoodsComparisonReportVO;
import cn.lili.modules.statistics.entity.vo.GoodsSalesSummaryReportVO;
import cn.lili.modules.statistics.entity.vo.SalesOrderDetailReportVO;
import cn.lili.modules.statistics.entity.vo.StorePerformanceReportVO;
import cn.lili.modules.statistics.service.SalesReportService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端销售报表
 *
 * @author Bulbasaur
 * @since 2026/07/20
 */
@Tag(name = "管理端,销售报表")
@RestController
@RequestMapping("/manager/statistics/report")
public class SalesReportManagerController {

    @Autowired
    private SalesReportService salesReportService;

    @Operation(summary = "店铺业绩报表")
    @GetMapping("/store-performance")
    public ResultMessage<IPage<StorePerformanceReportVO>> storePerformance(SalesReportQueryParam param, PageVO pageVO) {
        return ResultUtil.data(salesReportService.storePerformance(param, pageVO));
    }

    @Operation(summary = "导出店铺业绩报表")
    @GetMapping(value = "/store-performance/export", produces = "application/octet-stream")
    public void exportStorePerformance(SalesReportQueryParam param) {
        salesReportService.exportStorePerformance(ThreadContextHolder.getHttpResponse(), param);
    }

    @Operation(summary = "商品同比环比报表")
    @GetMapping("/goods-comparison")
    public ResultMessage<IPage<GoodsComparisonReportVO>> goodsComparison(SalesReportQueryParam param, PageVO pageVO) {
        return ResultUtil.data(salesReportService.goodsComparison(param, pageVO));
    }

    @Operation(summary = "导出商品同比环比报表")
    @GetMapping(value = "/goods-comparison/export", produces = "application/octet-stream")
    public void exportGoodsComparison(SalesReportQueryParam param) {
        salesReportService.exportGoodsComparison(ThreadContextHolder.getHttpResponse(), param);
    }

    @Operation(summary = "销售订单明细报表")
    @GetMapping("/sales-order-detail")
    public ResultMessage<IPage<SalesOrderDetailReportVO>> salesOrderDetail(SalesReportQueryParam param, PageVO pageVO) {
        return ResultUtil.data(salesReportService.salesOrderDetail(param, pageVO));
    }

    @Operation(summary = "导出销售订单明细报表")
    @GetMapping(value = "/sales-order-detail/export", produces = "application/octet-stream")
    public void exportSalesOrderDetail(SalesReportQueryParam param) {
        salesReportService.exportSalesOrderDetail(ThreadContextHolder.getHttpResponse(), param);
    }

    @Operation(summary = "商品销售汇总报表")
    @GetMapping("/goods-sales-summary")
    public ResultMessage<IPage<GoodsSalesSummaryReportVO>> goodsSalesSummary(SalesReportQueryParam param, PageVO pageVO) {
        return ResultUtil.data(salesReportService.goodsSalesSummary(param, pageVO));
    }

    @Operation(summary = "导出商品销售汇总报表")
    @GetMapping(value = "/goods-sales-summary/export", produces = "application/octet-stream")
    public void exportGoodsSalesSummary(SalesReportQueryParam param) {
        salesReportService.exportGoodsSalesSummary(ThreadContextHolder.getHttpResponse(), param);
    }
}
