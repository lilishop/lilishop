package cn.lili.modules.statistics.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.statistics.entity.dto.SalesReportQueryParam;
import cn.lili.modules.statistics.entity.vo.GoodsComparisonReportVO;
import cn.lili.modules.statistics.entity.vo.GoodsSalesSummaryReportVO;
import cn.lili.modules.statistics.entity.vo.SalesOrderDetailReportVO;
import cn.lili.modules.statistics.entity.vo.StorePerformanceReportVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import jakarta.servlet.http.HttpServletResponse;

/**
 * 销售报表服务
 *
 * @author Bulbasaur
 * @since 2026/07/20
 */
public interface SalesReportService {

    /**
     * 店铺业绩报表分页查询
     */
    IPage<StorePerformanceReportVO> storePerformance(SalesReportQueryParam param, PageVO pageVO);

    /**
     * 商品同比环比报表分页查询
     */
    IPage<GoodsComparisonReportVO> goodsComparison(SalesReportQueryParam param, PageVO pageVO);

    /**
     * 销售订单明细报表分页查询
     */
    IPage<SalesOrderDetailReportVO> salesOrderDetail(SalesReportQueryParam param, PageVO pageVO);

    /**
     * 商品销售汇总报表分页查询
     */
    IPage<GoodsSalesSummaryReportVO> goodsSalesSummary(SalesReportQueryParam param, PageVO pageVO);

    /**
     * 导出店铺业绩报表
     */
    void exportStorePerformance(HttpServletResponse response, SalesReportQueryParam param);

    /**
     * 导出商品同比环比报表
     */
    void exportGoodsComparison(HttpServletResponse response, SalesReportQueryParam param);

    /**
     * 导出销售订单明细报表
     */
    void exportSalesOrderDetail(HttpServletResponse response, SalesReportQueryParam param);

    /**
     * 导出商品销售汇总报表
     */
    void exportGoodsSalesSummary(HttpServletResponse response, SalesReportQueryParam param);
}
