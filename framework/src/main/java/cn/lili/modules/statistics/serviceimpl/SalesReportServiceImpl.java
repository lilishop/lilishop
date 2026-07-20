package cn.lili.modules.statistics.serviceimpl;

import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.finance.export.FinanceExportHelper;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.StoreService;
import cn.lili.modules.statistics.entity.dto.SalesReportQueryParam;
import cn.lili.modules.statistics.entity.vo.GoodsComparisonReportVO;
import cn.lili.modules.statistics.entity.vo.GoodsSalesRangeVO;
import cn.lili.modules.statistics.entity.vo.GoodsSalesSummaryReportVO;
import cn.lili.modules.statistics.entity.vo.OverViewMetricVO;
import cn.lili.modules.statistics.entity.vo.SalesOrderDetailReportVO;
import cn.lili.modules.statistics.entity.vo.StoreOrderStatsVO;
import cn.lili.modules.statistics.entity.vo.StorePerformanceReportVO;
import cn.lili.modules.statistics.mapper.SalesReportMapper;
import cn.lili.modules.statistics.service.SalesReportService;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * 销售报表服务实现
 *
 * @author Bulbasaur
 * @since 2026/07/20
 */
@Service
public class SalesReportServiceImpl extends ServiceImpl<SalesReportMapper, StoreFlow> implements SalesReportService {

    private static final String SORT_NUM = "NUM";

    @Autowired
    private StoreService storeService;

    @Override
    public IPage<StorePerformanceReportVO> storePerformance(SalesReportQueryParam param, PageVO pageVO) {
        return buildStorePerformance(param, PageUtil.initPage(pageVO));
    }

    private IPage<StorePerformanceReportVO> buildStorePerformance(SalesReportQueryParam param, Page<StorePerformanceReportVO> page) {
        Date[] currentDates = StatisticsDateUtil.getDateArray(param);
        Date[] momDates = StatisticsDateUtil.getPreviousDateArray(currentDates);
        Date[] yoyDates = StatisticsDateUtil.getYearOnYearDateArray(currentDates);
        boolean byDay = Boolean.TRUE.equals(param.getGroupByDay());

        QueryWrapper<StorePerformanceReportVO> totalQw = buildStoreFlowWrapper(param, currentDates);
        double totalOperatingIncome = nullSafeDouble(this.baseMapper.sumOperatingIncome(totalQw));

        QueryWrapper<StorePerformanceReportVO> qw = buildStoreFlowWrapper(param, currentDates);
        IPage<StorePerformanceReportVO> currentPage;
        if (byDay) {
            qw.groupBy("DATE_FORMAT(create_time,'%Y-%m-%d')");
            qw.orderByDesc("reportDate");
            currentPage = this.baseMapper.storePerformanceByDay(page, qw);
        } else {
            qw.groupBy("store_id");
            qw.orderByDesc("payAmount");
            currentPage = this.baseMapper.storePerformanceByStore(page, qw);
        }

        List<String> keys = currentPage.getRecords().stream()
                .map(row -> storePerformanceKey(row, byDay))
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        Map<String, StorePerformanceReportVO> momMap = loadStorePerformanceMap(param, momDates, keys, byDay);
        Map<String, StorePerformanceReportVO> yoyMap = loadStorePerformanceMap(param, yoyDates, keys, byDay);
        Map<String, Long> orderCountMap = loadStoreOrderCountMap(param, currentDates);
        Map<String, Long> uvMap = loadStoreUvMap(param, currentDates);

        List<StorePerformanceReportVO> rows = new ArrayList<>();
        for (StorePerformanceReportVO current : currentPage.getRecords()) {
            String key = storePerformanceKey(current, byDay);
            StorePerformanceReportVO mom = momMap.get(key);
            StorePerformanceReportVO yoy = yoyMap.get(key);
            String statsStoreId = current.getStoreId();
            enrichStorePerformanceRow(current, mom, yoy, totalOperatingIncome,
                    orderCountMap.getOrDefault(statsStoreId, 0L),
                    uvMap.getOrDefault(statsStoreId, 0L));
            rows.add(current);
        }
        fillStoreName(rows);
        return PageUtil.convertPage(currentPage, rows);
    }

    @Override
    public IPage<GoodsComparisonReportVO> goodsComparison(SalesReportQueryParam param, PageVO pageVO) {
        return buildGoodsComparison(param, PageUtil.initPage(pageVO));
    }

    private IPage<GoodsComparisonReportVO> buildGoodsComparison(SalesReportQueryParam param, Page<GoodsSalesRangeVO> page) {
        Date[] currentDates = resolveComparisonDates(param);
        Date[] momDates = StatisticsDateUtil.getPreviousDateArray(currentDates);
        Date[] yoyDates = StatisticsDateUtil.getYearOnYearDateArray(currentDates);
        String reportTime = formatReportTime(currentDates, param);

        QueryWrapper<GoodsSalesRangeVO> totalQw = buildGoodsRangeWrapper(param, currentDates);
        double totalSalesAmount = nullSafeDouble(this.baseMapper.sumSalesAmount(totalQw));
        long totalSalesNum = nullSafeLong(this.baseMapper.sumSalesNum(totalQw));

        QueryWrapper<GoodsSalesRangeVO> currentQw = buildGoodsRangeWrapper(param, currentDates);
        applyGoodsSort(currentQw, param);
        currentQw.groupBy("goods_id", "goods_name", "store_id");

        IPage<GoodsSalesRangeVO> currentPage = this.baseMapper.goodsSalesByRange(page, currentQw);
        List<String> keys = currentPage.getRecords().stream()
                .map(this::goodsStoreKey)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());

        Map<String, GoodsSalesRangeVO> momMap = loadGoodsRangeMap(param, momDates, keys);
        Map<String, GoodsSalesRangeVO> yoyMap = loadGoodsRangeMap(param, yoyDates, keys);

        List<GoodsComparisonReportVO> rows = new ArrayList<>();
        for (GoodsSalesRangeVO current : currentPage.getRecords()) {
            rows.add(buildGoodsComparisonRow(current, momMap, yoyMap, reportTime, totalSalesAmount, totalSalesNum));
        }
        fillGoodsComparisonStoreName(rows);
        return PageUtil.convertPage(currentPage, rows);
    }

    @Override
    public IPage<SalesOrderDetailReportVO> salesOrderDetail(SalesReportQueryParam param, PageVO pageVO) {
        Date[] dates = StatisticsDateUtil.getDateArray(param);
        QueryWrapper<SalesOrderDetailReportVO> qw = buildOrderDetailWrapper(param, dates);
        qw.orderByDesc("sf.create_time");
        IPage<SalesOrderDetailReportVO> page = this.baseMapper.salesOrderDetail(PageUtil.initPage(pageVO), qw);
        page.getRecords().forEach(this::fillOrderDetailMetrics);
        return page;
    }

    @Override
    public IPage<GoodsSalesSummaryReportVO> goodsSalesSummary(SalesReportQueryParam param, PageVO pageVO) {
        Date[] dates = StatisticsDateUtil.getDateArray(param);
        QueryWrapper<GoodsSalesSummaryReportVO> baseQw = buildGoodsSummaryWrapper(param, dates);
        double totalNetAmount = nullSafeDouble(this.baseMapper.sumGoodsNetSalesAmount(baseQw));

        QueryWrapper<GoodsSalesSummaryReportVO> qw = buildGoodsSummaryWrapper(param, dates);
        applyGoodsSummarySort(qw, param);
        qw.groupBy("goods_id", "goods_name");
        IPage<GoodsSalesSummaryReportVO> page = this.baseMapper.goodsSalesSummary(PageUtil.initPage(pageVO), qw);
        page.getRecords().forEach(row -> fillGoodsSummaryMetrics(row, totalNetAmount));
        return page;
    }

    @Override
    public void exportStorePerformance(HttpServletResponse response, SalesReportQueryParam param) {
        List<StorePerformanceReportVO> data = listStorePerformance(param);
        FinanceExportHelper.checkRowLimit(data.size());
        FinanceExportHelper.writeExcel(response, "店铺业绩报表", writer -> {
            FinanceExportHelper.useSheet(writer, "店铺业绩报表");
            writer.writeHeadRow(List.of("店铺名称", "订单支付笔数", "支付金额", "营业额", "优惠金额", "折扣率",
                    "营业收入", "营业收入占比", "笔单价", "退款笔数", "订单退款金额", "下单转化率", "支付转化率",
                    "环比差额", "环比增长率", "同比差额", "同比增长率"));
            writeRows(writer, data.stream().map(row -> List.<Object>of(
                    row.getStoreName(), row.getPayOrderCount(), row.getPayAmount(), row.getTurnover(),
                    row.getDiscountAmount(), formatPercent(row.getDiscountRate()), row.getOperatingIncome(),
                    formatPercent(row.getOperatingIncomePercent()), row.getAvgOrderPrice(), row.getRefundCount(),
                    row.getRefundAmount(), row.getOrderConversionRate(), row.getPayConversionRate(),
                    row.getMomDiff(), row.getMomRate(), row.getYoyDiff(), row.getYoyRate()
            )).toList());
        });
    }

    @Override
    public void exportGoodsComparison(HttpServletResponse response, SalesReportQueryParam param) {
        List<GoodsComparisonReportVO> data = listGoodsComparison(param);
        FinanceExportHelper.checkRowLimit(data.size());
        FinanceExportHelper.writeExcel(response, "商品同比环比报表", writer -> {
            FinanceExportHelper.useSheet(writer, "商品同比环比报表");
            writer.writeHeadRow(List.of("商品名称", "时间", "商品销售金额", "商品销售总金额", "占比",
                    "环比", "环比率", "环比差额", "同比", "同比率", "同比差额", "商品销售数", "销售占比", "店铺名称"));
            writeRows(writer, data.stream().map(row -> List.<Object>of(
                    row.getGoodsName(), row.getReportTime(), row.getSalesAmount(), row.getTotalSalesAmount(),
                    formatPercent(row.getAmountPercent()), row.getMomAmount(), row.getMomRate(), row.getMomDiff(),
                    row.getYoyAmount(), row.getYoyRate(), row.getYoyDiff(), row.getSalesNum(),
                    formatPercent(row.getSalesNumPercent()), row.getStoreName()
            )).toList());
        });
    }

    @Override
    public void exportSalesOrderDetail(HttpServletResponse response, SalesReportQueryParam param) {
        Date[] dates = StatisticsDateUtil.getDateArray(param);
        QueryWrapper<SalesOrderDetailReportVO> qw = buildOrderDetailWrapper(param, dates);
        qw.orderByDesc("sf.create_time");
        List<SalesOrderDetailReportVO> data = this.baseMapper.salesOrderDetailList(qw);
        data.forEach(this::fillOrderDetailMetrics);
        FinanceExportHelper.checkRowLimit(data.size());
        FinanceExportHelper.writeExcel(response, "销售订单明细报表", writer -> {
            FinanceExportHelper.useSheet(writer, "销售订单明细");
            writer.writeHeadRow(List.of("订单编号|售后单号", "发生时间", "交易类型", "成交数量", "成交金额",
                    "售价金额", "优惠金额", "收款明细", "优惠明细", "订单来源"));
            writeRows(writer, data.stream().map(row -> List.<Object>of(
                    row.getOrderRefundSn(), row.getOccurTime(), row.getFlowTypeName(), row.getNum(),
                    row.getTransactionAmount(), row.getSalePriceAmount(), row.getDiscountAmount(),
                    row.getPaymentDetail(), row.getDiscountDetail(), formatClientType(row.getClientType())
            )).toList());
        });
    }

    @Override
    public void exportGoodsSalesSummary(HttpServletResponse response, SalesReportQueryParam param) {
        Date[] dates = StatisticsDateUtil.getDateArray(param);
        QueryWrapper<GoodsSalesSummaryReportVO> baseQw = buildGoodsSummaryWrapper(param, dates);
        double totalNetAmount = nullSafeDouble(this.baseMapper.sumGoodsNetSalesAmount(baseQw));

        QueryWrapper<GoodsSalesSummaryReportVO> qw = buildGoodsSummaryWrapper(param, dates);
        applyGoodsSummarySort(qw, param);
        qw.groupBy("goods_id", "goods_name");
        List<GoodsSalesSummaryReportVO> data = this.baseMapper.goodsSalesSummaryList(qw);
        data.forEach(row -> fillGoodsSummaryMetrics(row, totalNetAmount));
        FinanceExportHelper.checkRowLimit(data.size());
        FinanceExportHelper.writeExcel(response, "商品销售汇总报表", writer -> {
            FinanceExportHelper.useSheet(writer, "商品销售汇总");
            writer.writeHeadRow(List.of("销售商品", "商品ID", "商品销售数量", "商品销售金额", "商品退货数量",
                    "商品退款金额", "商品净销售数量", "商品净销售金额", "占净销售金额百分比", "商品平均单价",
                    "商品售价金额", "商品优惠金额"));
            writeRows(writer, data.stream().map(row -> List.<Object>of(
                    row.getGoodsName(), row.getGoodsId(), row.getSalesNum(), row.getSalesAmount(),
                    row.getRefundNum(), row.getRefundAmount(), row.getNetNum(), row.getNetAmount(),
                    formatPercent(row.getNetAmountPercent()), row.getAvgPrice(),
                    row.getSalePriceAmount(), row.getDiscountAmount()
            )).toList());
        });
    }

    private List<StorePerformanceReportVO> listStorePerformance(SalesReportQueryParam param) {
        Page<StorePerformanceReportVO> page = exportPage();
        return buildStorePerformance(param, page).getRecords();
    }

    private void enrichStorePerformanceRow(StorePerformanceReportVO row,
                                           StorePerformanceReportVO mom,
                                           StorePerformanceReportVO yoy,
                                           double totalOperatingIncome,
                                           long orderCount,
                                           long uvNum) {
        double payAmount = nullSafeDouble(row.getPayAmount());
        double refundAmount = nullSafeDouble(row.getRefundAmount());
        double discountAmount = nullSafeDouble(row.getDiscountAmount());
        long payOrderCount = nullSafeLong(row.getPayOrderCount());
        double turnover = payAmount + discountAmount;
        double operatingIncome = payAmount - refundAmount;

        row.setPayAmount(payAmount);
        row.setRefundAmount(refundAmount);
        row.setDiscountAmount(discountAmount);
        row.setPayOrderCount(payOrderCount);
        row.setRefundCount(nullSafeLong(row.getRefundCount()));
        row.setTurnover(turnover);
        row.setDiscountRate(calcPercent(discountAmount, turnover));
        row.setOperatingIncome(operatingIncome);
        row.setOperatingIncomePercent(calcPercent(operatingIncome, totalOperatingIncome));
        row.setAvgOrderPrice(payOrderCount == 0 ? 0D : payAmount / payOrderCount);
        row.setOrderConversionRate(formatConversionRate(orderCount, uvNum));
        row.setPayConversionRate(formatConversionRate(payOrderCount, orderCount));

        double momIncome = mom == null ? 0D : calcOperatingIncome(mom);
        double yoyIncome = yoy == null ? 0D : calcOperatingIncome(yoy);
        OverViewMetricVO momMetric = OverViewMetricVO.of(operatingIncome, momIncome);
        OverViewMetricVO yoyMetric = OverViewMetricVO.of(operatingIncome, yoyIncome);
        row.setMomDiff(operatingIncome - momIncome);
        row.setMomRate(momMetric.getRate());
        row.setYoyDiff(operatingIncome - yoyIncome);
        row.setYoyRate(yoyMetric.getRate());
    }

    private double calcOperatingIncome(StorePerformanceReportVO row) {
        return nullSafeDouble(row.getPayAmount()) - nullSafeDouble(row.getRefundAmount());
    }

    private Map<String, StorePerformanceReportVO> loadStorePerformanceMap(SalesReportQueryParam param,
                                                                        Date[] dates,
                                                                        List<String> keys,
                                                                        boolean byDay) {
        if (keys == null || keys.isEmpty()) {
            return Map.of();
        }
        QueryWrapper<StorePerformanceReportVO> qw = buildStoreFlowWrapper(param, dates);
        List<StorePerformanceReportVO> list;
        if (byDay) {
            qw.in("DATE_FORMAT(create_time,'%Y-%m-%d')", keys);
            qw.groupBy("DATE_FORMAT(create_time,'%Y-%m-%d')");
            list = this.baseMapper.storePerformanceByDayList(qw);
        } else {
            qw.in("store_id", keys);
            qw.groupBy("store_id");
            list = this.baseMapper.storePerformanceByStoreList(qw);
        }
        Map<String, StorePerformanceReportVO> map = new HashMap<>();
        for (StorePerformanceReportVO item : list) {
            map.put(storePerformanceKey(item, byDay), item);
        }
        return map;
    }

    private Map<String, Long> loadStoreOrderCountMap(SalesReportQueryParam param, Date[] dates) {
        QueryWrapper<StoreOrderStatsVO> qw = new QueryWrapper<>();
        qw.between("create_time", dates[0], dates[1]);
        qw.eq("delete_flag", false);
        qw.eq(StringUtils.isNotEmpty(param.getStoreId()), "store_id", param.getStoreId());
        List<StoreOrderStatsVO> list = this.baseMapper.storeOrderCountList(qw);
        Map<String, Long> map = new HashMap<>();
        for (StoreOrderStatsVO item : list) {
            map.put(item.getStoreId(), nullSafeLong(item.getOrderCount()));
        }
        return map;
    }

    private Map<String, Long> loadStoreUvMap(SalesReportQueryParam param, Date[] dates) {
        QueryWrapper<StoreOrderStatsVO> qw = new QueryWrapper<>();
        qw.between("date", dates[0], dates[1]);
        qw.ne("store_id", "-1");
        qw.eq(StringUtils.isNotEmpty(param.getStoreId()), "store_id", param.getStoreId());
        List<StoreOrderStatsVO> list = this.baseMapper.storeUvList(qw);
        Map<String, Long> map = new HashMap<>();
        for (StoreOrderStatsVO item : list) {
            map.put(item.getStoreId(), nullSafeLong(item.getUvNum()));
        }
        return map;
    }

    private String storePerformanceKey(StorePerformanceReportVO row, boolean byDay) {
        if (row == null) {
            return null;
        }
        if (byDay) {
            return row.getReportDate();
        }
        return row.getStoreId();
    }

    private String formatConversionRate(long numerator, long denominator) {
        if (denominator == 0L) {
            return "0%";
        }
        double rate = (double) numerator / denominator;
        if (rate > 1D) {
            rate = 1D;
        }
        return Math.round(rate * 100) + "%";
    }

    private List<GoodsComparisonReportVO> listGoodsComparison(SalesReportQueryParam param) {
        Page<GoodsSalesRangeVO> page = exportPage();
        return buildGoodsComparison(param, page).getRecords();
    }

    private GoodsComparisonReportVO buildGoodsComparisonRow(GoodsSalesRangeVO current,
                                                            Map<String, GoodsSalesRangeVO> momMap,
                                                            Map<String, GoodsSalesRangeVO> yoyMap,
                                                            String reportTime,
                                                            double totalSalesAmount,
                                                            long totalSalesNum) {
        GoodsComparisonReportVO vo = new GoodsComparisonReportVO();
        vo.setGoodsId(current.getGoodsId());
        vo.setGoodsName(current.getGoodsName());
        vo.setStoreId(current.getStoreId());
        vo.setReportTime(reportTime);

        double curAmount = nullSafeDouble(current.getSalesAmount());
        long curNum = nullSafeLong(current.getSalesNum());
        vo.setSalesAmount(curAmount);
        vo.setTotalSalesAmount(totalSalesAmount);
        vo.setAmountPercent(calcPercent(curAmount, totalSalesAmount));
        vo.setSalesNum(curNum);
        vo.setSalesNumPercent(calcPercent(curNum, totalSalesNum));

        GoodsSalesRangeVO mom = momMap.get(goodsStoreKey(current));
        double momAmount = mom == null ? 0D : nullSafeDouble(mom.getSalesAmount());
        OverViewMetricVO amountMom = OverViewMetricVO.of(curAmount, momAmount);
        vo.setMomAmount(momAmount);
        vo.setMomRate(amountMom.getRate());
        vo.setMomDiff(curAmount - momAmount);

        GoodsSalesRangeVO yoy = yoyMap.get(goodsStoreKey(current));
        double yoyAmount = yoy == null ? 0D : nullSafeDouble(yoy.getSalesAmount());
        OverViewMetricVO amountYoy = OverViewMetricVO.of(curAmount, yoyAmount);
        vo.setYoyAmount(yoyAmount);
        vo.setYoyRate(amountYoy.getRate());
        vo.setYoyDiff(curAmount - yoyAmount);
        return vo;
    }

    private Map<String, GoodsSalesRangeVO> loadGoodsRangeMap(SalesReportQueryParam param, Date[] dates, List<String> keys) {
        if (keys == null || keys.isEmpty()) {
            return Map.of();
        }
        List<String> goodsIds = keys.stream()
                .map(this::goodsIdFromKey)
                .filter(Objects::nonNull)
                .distinct()
                .collect(Collectors.toList());
        if (goodsIds.isEmpty()) {
            return Map.of();
        }
        QueryWrapper<GoodsSalesRangeVO> qw = buildGoodsRangeWrapper(param, dates);
        qw.in("goods_id", goodsIds);
        qw.groupBy("goods_id", "goods_name", "store_id");
        List<GoodsSalesRangeVO> list = this.baseMapper.goodsSalesByRangeList(qw);
        Map<String, GoodsSalesRangeVO> map = new HashMap<>();
        for (GoodsSalesRangeVO item : list) {
            map.put(goodsStoreKey(item), item);
        }
        return map;
    }

    private QueryWrapper<GoodsSalesRangeVO> buildGoodsRangeWrapper(SalesReportQueryParam param, Date[] dates) {
        QueryWrapper<GoodsSalesRangeVO> qw = new QueryWrapper<>();
        qw.between("create_time", dates[0], dates[1]);
        qw.eq(StringUtils.isNotEmpty(param.getStoreId()), "store_id", param.getStoreId());
        qw.eq(StringUtils.isNotEmpty(param.getCategoryId()), "category_id", param.getCategoryId());
        qw.eq(StringUtils.isNotEmpty(param.getGoodsId()), "goods_id", param.getGoodsId());
        if (StringUtils.isNotEmpty(param.getKeyword())) {
            qw.like("goods_name", param.getKeyword());
        }
        if (StringUtils.isNotEmpty(param.getBrandId())) {
            qw.apply("goods_id IN (SELECT id FROM li_goods WHERE brand_id = {0} AND delete_flag = 0)", param.getBrandId());
        }
        if (StringUtils.isNotEmpty(param.getClientType())) {
            qw.apply("order_sn IN (SELECT sn FROM li_order WHERE client_type = {0} AND delete_flag = 0)", param.getClientType());
        }
        return qw;
    }

    private Date[] resolveComparisonDates(SalesReportQueryParam param) {
        if ("DAY".equalsIgnoreCase(param.getReportDateType()) && param.getQueryDate() != null) {
            return StatisticsDateUtil.getDateArray(param.getQueryDate());
        }
        return StatisticsDateUtil.getDateArray(param);
    }

    private void applyGoodsSort(QueryWrapper<GoodsSalesRangeVO> qw, SalesReportQueryParam param) {
        if (SORT_NUM.equalsIgnoreCase(param.getSortType())) {
            qw.orderByDesc("salesNum");
        } else {
            qw.orderByDesc("salesAmount");
        }
    }

    private String goodsStoreKey(GoodsSalesRangeVO item) {
        if (item == null || item.getGoodsId() == null) {
            return null;
        }
        return item.getGoodsId() + "_" + Objects.toString(item.getStoreId(), "");
    }

    private String goodsIdFromKey(String key) {
        if (StringUtils.isEmpty(key)) {
            return null;
        }
        int idx = key.indexOf('_');
        return idx < 0 ? key : key.substring(0, idx);
    }

    private String formatReportTime(Date[] dates, SalesReportQueryParam param) {
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        if ("DAY".equalsIgnoreCase(param.getReportDateType())) {
            return sdf.format(dates[0]);
        }
        String start = sdf.format(dates[0]);
        String end = sdf.format(dates[1]);
        if (start.equals(end)) {
            return start;
        }
        return start + " ~ " + end;
    }

    private double calcPercent(double part, double total) {
        return total == 0D ? 0D : part / total * 100;
    }

    private double calcPercent(long part, long total) {
        return total == 0L ? 0D : (double) part / total * 100;
    }

    private String formatPercent(Double value) {
        if (value == null) {
            return "0%";
        }
        return String.format("%.2f%%", value);
    }

    private <T> Page<T> exportPage() {
        return new Page<>(1, FinanceExportHelper.MAX_EXPORT_ROWS);
    }

    private QueryWrapper<StorePerformanceReportVO> buildStoreFlowWrapper(SalesReportQueryParam param, Date[] dates) {
        QueryWrapper<StorePerformanceReportVO> qw = new QueryWrapper<>();
        qw.between("create_time", dates[0], dates[1]);
        qw.eq(StringUtils.isNotEmpty(param.getStoreId()), "store_id", param.getStoreId());
        return qw;
    }

    private QueryWrapper<GoodsSalesSummaryReportVO> buildGoodsSummaryWrapper(SalesReportQueryParam param, Date[] dates) {
        QueryWrapper<GoodsSalesSummaryReportVO> qw = new QueryWrapper<>();
        qw.between("create_time", dates[0], dates[1]);
        qw.eq(StringUtils.isNotEmpty(param.getStoreId()), "store_id", param.getStoreId());
        qw.eq(StringUtils.isNotEmpty(param.getCategoryId()), "category_id", param.getCategoryId());
        qw.eq(StringUtils.isNotEmpty(param.getGoodsId()), "goods_id", param.getGoodsId());
        if (StringUtils.isNotEmpty(param.getKeyword())) {
            qw.like("goods_name", param.getKeyword());
        }
        return qw;
    }

    private QueryWrapper<SalesOrderDetailReportVO> buildOrderDetailWrapper(SalesReportQueryParam param, Date[] dates) {
        QueryWrapper<SalesOrderDetailReportVO> qw = new QueryWrapper<>();
        qw.between("sf.create_time", dates[0], dates[1]);
        qw.eq(StringUtils.isNotEmpty(param.getStoreId()), "sf.store_id", param.getStoreId());
        qw.eq(StringUtils.isNotEmpty(param.getOrderSn()), "sf.order_sn", param.getOrderSn());
        qw.like(StringUtils.isNotEmpty(param.getMemberName()), "sf.member_name", param.getMemberName());
        qw.eq(StringUtils.isNotEmpty(param.getClientType()), "o.client_type", param.getClientType());
        if (StringUtils.isNotEmpty(param.getKeyword())) {
            qw.like("sf.goods_name", param.getKeyword());
        }
        if (StringUtils.isNotEmpty(param.getGoodsId())) {
            qw.eq("sf.goods_id", param.getGoodsId());
        }
        return qw;
    }

    private void fillOrderDetailMetrics(SalesOrderDetailReportVO row) {
        double transactionAmount = nullSafeDouble(row.getTransactionAmount());
        double siteCoupon = nullSafeDouble(row.getSiteCouponPrice());
        double flashDiscount = nullSafeDouble(row.getFlashDiscountSubsidy());
        double nthSubsidy = nullSafeDouble(row.getNthItemSubsidy());
        double discountAmount = siteCoupon + flashDiscount + nthSubsidy;

        row.setTransactionAmount(transactionAmount);
        row.setDiscountAmount(discountAmount);
        row.setSalePriceAmount(transactionAmount + discountAmount);
        row.setOrderRefundSn(buildOrderRefundSn(row.getOrderSn(), row.getRefundSn()));
        row.setFlowTypeName(formatFlowType(row.getFlowType()));
        row.setPaymentDetail(formatPaymentDetail(row.getPaymentName()));
        row.setDiscountDetail(buildDiscountDetail(siteCoupon, flashDiscount, nthSubsidy));
    }

    private String buildOrderRefundSn(String orderSn, String refundSn) {
        if (StringUtils.isNotEmpty(refundSn)) {
            return StringUtils.isNotEmpty(orderSn) ? orderSn + " | " + refundSn : refundSn;
        }
        return orderSn;
    }

    private String formatFlowType(String flowType) {
        if ("PAY".equalsIgnoreCase(flowType)) {
            return "支付";
        }
        if ("REFUND".equalsIgnoreCase(flowType)) {
            return "退款";
        }
        return flowType;
    }

    private String formatPaymentDetail(String paymentName) {
        if (StringUtils.isEmpty(paymentName)) {
            return "";
        }
        return switch (paymentName) {
            case "WECHAT" -> "微信支付";
            case "ALIPAY" -> "支付宝";
            case "WALLET" -> "余额支付";
            case "BANK_TRANSFER" -> "线下转账";
            default -> paymentName;
        };
    }

    private String buildDiscountDetail(double siteCoupon, double flashDiscount, double nthSubsidy) {
        List<String> parts = new ArrayList<>();
        if (siteCoupon > 0D) {
            parts.add("平台券:" + siteCoupon);
        }
        if (flashDiscount > 0D) {
            parts.add("限时直降:" + flashDiscount);
        }
        if (nthSubsidy > 0D) {
            parts.add("第N件优惠:" + nthSubsidy);
        }
        return String.join(";", parts);
    }

    private String formatClientType(String clientType) {
        if (StringUtils.isEmpty(clientType)) {
            return "";
        }
        return switch (clientType) {
            case "H5" -> "移动端";
            case "PC" -> "PC端";
            case "WECHAT_MP" -> "小程序";
            case "APP" -> "移动应用端";
            case "UNKNOWN" -> "未知";
            default -> clientType;
        };
    }

    private void applyGoodsSummarySort(QueryWrapper<GoodsSalesSummaryReportVO> qw, SalesReportQueryParam param) {
        if (SORT_NUM.equalsIgnoreCase(param.getSortType())) {
            qw.orderByDesc("salesNum");
        } else {
            qw.orderByDesc("salesAmount");
        }
    }

    private void fillGoodsSummaryMetrics(GoodsSalesSummaryReportVO row, double totalNetAmount) {
        long salesNum = nullSafeLong(row.getSalesNum());
        double salesAmount = nullSafeDouble(row.getSalesAmount());
        long refundNum = nullSafeLong(row.getRefundNum());
        double refundAmount = nullSafeDouble(row.getRefundAmount());
        double discountAmount = nullSafeDouble(row.getDiscountAmount());
        double netAmount = salesAmount - refundAmount;

        row.setSalesNum(salesNum);
        row.setSalesAmount(salesAmount);
        row.setRefundNum(refundNum);
        row.setRefundAmount(refundAmount);
        row.setDiscountAmount(discountAmount);
        row.setNetNum(Math.max(0L, salesNum - refundNum));
        row.setNetAmount(netAmount);
        row.setNetAmountPercent(calcPercent(netAmount, totalNetAmount));
        row.setAvgPrice(salesNum == 0 ? 0D : salesAmount / salesNum);
        row.setSalePriceAmount(salesAmount + discountAmount);
    }

    private void fillStoreName(List<StorePerformanceReportVO> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        Map<String, String> nameMap = loadStoreNameMap(list.stream()
                .map(StorePerformanceReportVO::getStoreId)
                .collect(Collectors.toList()));
        for (StorePerformanceReportVO vo : list) {
            if (vo.getStoreId() == null) {
                continue;
            }
            String currentName = nameMap.get(vo.getStoreId());
            if (StringUtils.isNotEmpty(currentName)) {
                vo.setStoreName(currentName);
            }
        }
    }

    private void fillGoodsComparisonStoreName(List<GoodsComparisonReportVO> list) {
        if (list == null || list.isEmpty()) {
            return;
        }
        Map<String, String> nameMap = loadStoreNameMap(list.stream()
                .map(GoodsComparisonReportVO::getStoreId)
                .collect(Collectors.toList()));
        for (GoodsComparisonReportVO vo : list) {
            if (vo.getStoreId() == null) {
                continue;
            }
            String currentName = nameMap.get(vo.getStoreId());
            if (StringUtils.isNotEmpty(currentName)) {
                vo.setStoreName(currentName);
            }
        }
    }

    private Map<String, String> loadStoreNameMap(List<String> storeIds) {
        List<String> ids = storeIds == null ? List.of() : storeIds.stream()
                .filter(Objects::nonNull)
                .distinct()
                .collect(Collectors.toList());
        if (ids.isEmpty()) {
            return Map.of();
        }
        Map<String, String> nameMap = new HashMap<>();
        List<Store> stores = storeService.listByIds(ids);
        if (stores != null) {
            for (Store store : stores) {
                nameMap.put(store.getId(), store.getStoreName());
            }
        }
        return nameMap;
    }

    private void writeRows(cn.hutool.poi.excel.ExcelWriter writer, List<List<Object>> rows) {
        if (rows != null && !rows.isEmpty()) {
            writer.write(rows, false);
        }
    }

    private double nullSafeDouble(Double value) {
        return value == null ? 0D : value;
    }

    private long nullSafeLong(Long value) {
        return value == null ? 0L : value;
    }
}
