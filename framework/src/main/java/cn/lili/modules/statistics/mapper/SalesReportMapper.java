package cn.lili.modules.statistics.mapper;

import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.statistics.entity.vo.GoodsSalesRangeVO;
import cn.lili.modules.statistics.entity.vo.GoodsSalesSummaryReportVO;
import cn.lili.modules.statistics.entity.vo.SalesOrderDetailReportVO;
import cn.lili.modules.statistics.entity.vo.StoreOrderStatsVO;
import cn.lili.modules.statistics.entity.vo.StorePerformanceReportVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 销售报表数据层
 *
 * @author Bulbasaur
 * @since 2026/07/20
 */
public interface SalesReportMapper extends BaseMapper<StoreFlow> {

    /**
     * 店铺业绩报表(按店铺聚合)
     */
    @Select("SELECT store_id AS storeId, MAX(store_name) AS storeName," +
            " SUM(CASE WHEN flow_type='PAY' THEN final_price ELSE 0 END) AS payAmount," +
            " SUM(CASE WHEN flow_type='REFUND' THEN final_price ELSE 0 END) AS refundAmount," +
            " COUNT(DISTINCT CASE WHEN flow_type='PAY' THEN order_sn END) AS payOrderCount," +
            " COUNT(DISTINCT CASE WHEN flow_type='REFUND' THEN order_sn END) AS refundCount," +
            " SUM(CASE WHEN flow_type='PAY' THEN COALESCE(site_coupon_price,0) + COALESCE(flash_discount_subsidy,0)" +
            " + COALESCE(nth_item_subsidy,0) ELSE 0 END) AS discountAmount" +
            " FROM li_store_flow ${ew.customSqlSegment}")
    IPage<StorePerformanceReportVO> storePerformanceByStore(IPage<StorePerformanceReportVO> page,
                                                            @Param(Constants.WRAPPER) Wrapper<StorePerformanceReportVO> queryWrapper);

    /**
     * 店铺业绩报表(按店铺聚合,列表)
     */
    @Select("SELECT store_id AS storeId, MAX(store_name) AS storeName," +
            " SUM(CASE WHEN flow_type='PAY' THEN final_price ELSE 0 END) AS payAmount," +
            " SUM(CASE WHEN flow_type='REFUND' THEN final_price ELSE 0 END) AS refundAmount," +
            " COUNT(DISTINCT CASE WHEN flow_type='PAY' THEN order_sn END) AS payOrderCount," +
            " COUNT(DISTINCT CASE WHEN flow_type='REFUND' THEN order_sn END) AS refundCount," +
            " SUM(CASE WHEN flow_type='PAY' THEN COALESCE(site_coupon_price,0) + COALESCE(flash_discount_subsidy,0)" +
            " + COALESCE(nth_item_subsidy,0) ELSE 0 END) AS discountAmount" +
            " FROM li_store_flow ${ew.customSqlSegment}")
    List<StorePerformanceReportVO> storePerformanceByStoreList(@Param(Constants.WRAPPER) Wrapper<StorePerformanceReportVO> queryWrapper);

    /**
     * 店铺业绩报表(按日聚合)
     */
    @Select("SELECT DATE_FORMAT(create_time,'%Y-%m-%d') AS reportDate, MAX(store_id) AS storeId," +
            " MAX(store_name) AS storeName," +
            " SUM(CASE WHEN flow_type='PAY' THEN final_price ELSE 0 END) AS payAmount," +
            " SUM(CASE WHEN flow_type='REFUND' THEN final_price ELSE 0 END) AS refundAmount," +
            " COUNT(DISTINCT CASE WHEN flow_type='PAY' THEN order_sn END) AS payOrderCount," +
            " COUNT(DISTINCT CASE WHEN flow_type='REFUND' THEN order_sn END) AS refundCount," +
            " SUM(CASE WHEN flow_type='PAY' THEN COALESCE(site_coupon_price,0) + COALESCE(flash_discount_subsidy,0)" +
            " + COALESCE(nth_item_subsidy,0) ELSE 0 END) AS discountAmount" +
            " FROM li_store_flow ${ew.customSqlSegment}")
    IPage<StorePerformanceReportVO> storePerformanceByDay(IPage<StorePerformanceReportVO> page,
                                                          @Param(Constants.WRAPPER) Wrapper<StorePerformanceReportVO> queryWrapper);

    /**
     * 店铺业绩报表(按日聚合,列表)
     */
    @Select("SELECT DATE_FORMAT(create_time,'%Y-%m-%d') AS reportDate, MAX(store_id) AS storeId," +
            " MAX(store_name) AS storeName," +
            " SUM(CASE WHEN flow_type='PAY' THEN final_price ELSE 0 END) AS payAmount," +
            " SUM(CASE WHEN flow_type='REFUND' THEN final_price ELSE 0 END) AS refundAmount," +
            " COUNT(DISTINCT CASE WHEN flow_type='PAY' THEN order_sn END) AS payOrderCount," +
            " COUNT(DISTINCT CASE WHEN flow_type='REFUND' THEN order_sn END) AS refundCount," +
            " SUM(CASE WHEN flow_type='PAY' THEN COALESCE(site_coupon_price,0) + COALESCE(flash_discount_subsidy,0)" +
            " + COALESCE(nth_item_subsidy,0) ELSE 0 END) AS discountAmount" +
            " FROM li_store_flow ${ew.customSqlSegment}")
    List<StorePerformanceReportVO> storePerformanceByDayList(@Param(Constants.WRAPPER) Wrapper<StorePerformanceReportVO> queryWrapper);

    /**
     * 汇总营业收入(支付金额-退款金额)
     */
    @Select("SELECT COALESCE(SUM(CASE WHEN flow_type='PAY' THEN final_price ELSE 0 END),0)" +
            " - COALESCE(SUM(CASE WHEN flow_type='REFUND' THEN final_price ELSE 0 END),0)" +
            " FROM li_store_flow ${ew.customSqlSegment}")
    Double sumOperatingIncome(@Param(Constants.WRAPPER) Wrapper<StorePerformanceReportVO> queryWrapper);

    /**
     * 店铺下单笔数统计
     */
    @Select("SELECT store_id AS storeId, COUNT(*) AS orderCount FROM li_order ${ew.customSqlSegment} GROUP BY store_id")
    List<StoreOrderStatsVO> storeOrderCountList(@Param(Constants.WRAPPER) Wrapper<StoreOrderStatsVO> queryWrapper);

    /**
     * 店铺访客数统计
     */
    @Select("SELECT store_id AS storeId, COALESCE(SUM(uv_num),0) AS uvNum FROM li_s_platform_view_data ${ew.customSqlSegment} GROUP BY store_id")
    List<StoreOrderStatsVO> storeUvList(@Param(Constants.WRAPPER) Wrapper<StoreOrderStatsVO> queryWrapper);

    /**
     * 商品区间销售聚合(分页)
     */
    @Select("SELECT goods_id AS goodsId, MAX(goods_name) AS goodsName, MAX(store_id) AS storeId," +
            " SUM(CASE WHEN flow_type='PAY' THEN final_price ELSE 0 END) AS salesAmount," +
            " SUM(CASE WHEN flow_type='PAY' THEN num ELSE 0 END) AS salesNum" +
            " FROM li_store_flow ${ew.customSqlSegment}")
    IPage<GoodsSalesRangeVO> goodsSalesByRange(IPage<GoodsSalesRangeVO> page,
                                               @Param(Constants.WRAPPER) Wrapper<GoodsSalesRangeVO> queryWrapper);

    /**
     * 商品区间销售聚合(列表)
     */
    @Select("SELECT goods_id AS goodsId, MAX(goods_name) AS goodsName, MAX(store_id) AS storeId," +
            " SUM(CASE WHEN flow_type='PAY' THEN final_price ELSE 0 END) AS salesAmount," +
            " SUM(CASE WHEN flow_type='PAY' THEN num ELSE 0 END) AS salesNum" +
            " FROM li_store_flow ${ew.customSqlSegment}")
    List<GoodsSalesRangeVO> goodsSalesByRangeList(@Param(Constants.WRAPPER) Wrapper<GoodsSalesRangeVO> queryWrapper);

    /**
     * 汇总商品销售金额
     */
    @Select("SELECT COALESCE(SUM(CASE WHEN flow_type='PAY' THEN final_price ELSE 0 END),0)" +
            " FROM li_store_flow ${ew.customSqlSegment}")
    Double sumSalesAmount(@Param(Constants.WRAPPER) Wrapper<GoodsSalesRangeVO> queryWrapper);

    /**
     * 汇总商品销售数量
     */
    @Select("SELECT COALESCE(SUM(CASE WHEN flow_type='PAY' THEN num ELSE 0 END),0)" +
            " FROM li_store_flow ${ew.customSqlSegment}")
    Long sumSalesNum(@Param(Constants.WRAPPER) Wrapper<GoodsSalesRangeVO> queryWrapper);

    /**
     * 商品销售汇总报表(分页)
     */
    @Select("SELECT goods_id AS goodsId, MAX(goods_name) AS goodsName," +
            " SUM(CASE WHEN flow_type='PAY' THEN num ELSE 0 END) AS salesNum," +
            " SUM(CASE WHEN flow_type='PAY' THEN final_price ELSE 0 END) AS salesAmount," +
            " SUM(CASE WHEN flow_type='REFUND' THEN num ELSE 0 END) AS refundNum," +
            " SUM(CASE WHEN flow_type='REFUND' THEN final_price ELSE 0 END) AS refundAmount," +
            " SUM(CASE WHEN flow_type='PAY' THEN COALESCE(site_coupon_price,0) + COALESCE(flash_discount_subsidy,0)" +
            " + COALESCE(nth_item_subsidy,0) ELSE 0 END) AS discountAmount" +
            " FROM li_store_flow ${ew.customSqlSegment}")
    IPage<GoodsSalesSummaryReportVO> goodsSalesSummary(IPage<GoodsSalesSummaryReportVO> page,
                                                         @Param(Constants.WRAPPER) Wrapper<GoodsSalesSummaryReportVO> queryWrapper);

    /**
     * 商品销售汇总报表(列表)
     */
    @Select("SELECT goods_id AS goodsId, MAX(goods_name) AS goodsName," +
            " SUM(CASE WHEN flow_type='PAY' THEN num ELSE 0 END) AS salesNum," +
            " SUM(CASE WHEN flow_type='PAY' THEN final_price ELSE 0 END) AS salesAmount," +
            " SUM(CASE WHEN flow_type='REFUND' THEN num ELSE 0 END) AS refundNum," +
            " SUM(CASE WHEN flow_type='REFUND' THEN final_price ELSE 0 END) AS refundAmount," +
            " SUM(CASE WHEN flow_type='PAY' THEN COALESCE(site_coupon_price,0) + COALESCE(flash_discount_subsidy,0)" +
            " + COALESCE(nth_item_subsidy,0) ELSE 0 END) AS discountAmount" +
            " FROM li_store_flow ${ew.customSqlSegment}")
    List<GoodsSalesSummaryReportVO> goodsSalesSummaryList(@Param(Constants.WRAPPER) Wrapper<GoodsSalesSummaryReportVO> queryWrapper);

    /**
     * 汇总商品净销售金额(支付金额-退款金额)
     */
    @Select("SELECT COALESCE(SUM(CASE WHEN flow_type='PAY' THEN final_price ELSE 0 END),0)" +
            " - COALESCE(SUM(CASE WHEN flow_type='REFUND' THEN final_price ELSE 0 END),0)" +
            " FROM li_store_flow ${ew.customSqlSegment}")
    Double sumGoodsNetSalesAmount(@Param(Constants.WRAPPER) Wrapper<GoodsSalesSummaryReportVO> queryWrapper);

    /**
     * 销售订单明细报表(分页)
     */
    @Select("SELECT sf.order_sn AS orderSn, sf.refund_sn AS refundSn, sf.create_time AS occurTime," +
            " sf.flow_type AS flowType, sf.num AS num, sf.final_price AS transactionAmount," +
            " sf.payment_name AS paymentName, o.client_type AS clientType," +
            " COALESCE(sf.site_coupon_price,0) AS siteCouponPrice," +
            " COALESCE(sf.flash_discount_subsidy,0) AS flashDiscountSubsidy," +
            " COALESCE(sf.nth_item_subsidy,0) AS nthItemSubsidy" +
            " FROM li_store_flow sf LEFT JOIN li_order o ON sf.order_sn = o.sn ${ew.customSqlSegment}")
    IPage<SalesOrderDetailReportVO> salesOrderDetail(IPage<SalesOrderDetailReportVO> page,
                                                     @Param(Constants.WRAPPER) Wrapper<SalesOrderDetailReportVO> queryWrapper);

    /**
     * 销售订单明细报表(列表)
     */
    @Select("SELECT sf.order_sn AS orderSn, sf.refund_sn AS refundSn, sf.create_time AS occurTime," +
            " sf.flow_type AS flowType, sf.num AS num, sf.final_price AS transactionAmount," +
            " sf.payment_name AS paymentName, o.client_type AS clientType," +
            " COALESCE(sf.site_coupon_price,0) AS siteCouponPrice," +
            " COALESCE(sf.flash_discount_subsidy,0) AS flashDiscountSubsidy," +
            " COALESCE(sf.nth_item_subsidy,0) AS nthItemSubsidy" +
            " FROM li_store_flow sf LEFT JOIN li_order o ON sf.order_sn = o.sn ${ew.customSqlSegment}")
    List<SalesOrderDetailReportVO> salesOrderDetailList(@Param(Constants.WRAPPER) Wrapper<SalesOrderDetailReportVO> queryWrapper);
}
