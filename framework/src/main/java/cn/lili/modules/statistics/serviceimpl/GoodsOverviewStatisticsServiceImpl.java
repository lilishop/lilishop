package cn.lili.modules.statistics.serviceimpl;

import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.enums.RefundStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.GoodsOverviewVO;
import cn.lili.modules.statistics.entity.vo.GoodsRankVO;
import cn.lili.modules.statistics.entity.vo.OverViewMetricVO;
import cn.lili.modules.statistics.mapper.OrderStatisticsMapper;
import cn.lili.modules.statistics.service.GoodsOverviewStatisticsService;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 商品概况统计业务层实现
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Service
public class GoodsOverviewStatisticsServiceImpl implements GoodsOverviewStatisticsService {

    @Autowired
    private OrderStatisticsMapper orderStatisticsMapper;
    @Autowired
    private OrderItemService orderItemService;

    @Override
    public GoodsOverviewVO overview(StatisticsQueryParam statisticsQueryParam) {
        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);
        Date[] previous = StatisticsDateUtil.getPreviousDateArray(dates);

        PeriodMetrics cur = calc(dates, statisticsQueryParam.getStoreId());
        PeriodMetrics last = calc(previous, statisticsQueryParam.getStoreId());

        GoodsOverviewVO vo = new GoodsOverviewVO();
        vo.setSalesAmount(OverViewMetricVO.of(cur.salesAmount, last.salesAmount));
        vo.setRefundAmount(OverViewMetricVO.of(cur.refundAmount, last.refundAmount));
        vo.setNetSalesAmount(OverViewMetricVO.of(cur.netSalesAmount, last.netSalesAmount));
        vo.setDiscountAmount(OverViewMetricVO.of(cur.discountAmount, last.discountAmount));
        vo.setSalePriceAmount(OverViewMetricVO.of(cur.salePriceAmount, last.salePriceAmount));
        vo.setSalesNum(OverViewMetricVO.of(cur.salesNum, last.salesNum));
        vo.setRefundNum(OverViewMetricVO.of(cur.refundNum, last.refundNum));
        vo.setNetSalesNum(OverViewMetricVO.of(cur.netSalesNum, last.netSalesNum));
        return vo;
    }

    private PeriodMetrics calc(Date[] dates, String storeId) {
        PeriodMetrics m = new PeriodMetrics();
        QueryWrapper salesQw = Wrappers.query();
        salesQw.between("o.payment_time", dates[0], dates[1]);
        salesQw.eq("o.pay_status", PayStatusEnum.PAID.name());
        salesQw.eq(StringUtils.isNotEmpty(storeId), "o.store_id", storeId);
        m.salesAmount = nullToZero(orderStatisticsMapper.sumGoodsSalesAmount(salesQw));
        m.salesNum = nullToZero(orderStatisticsMapper.sumGoodsSalesNum(salesQw));

        QueryWrapper refundQw = Wrappers.query();
        refundQw.eq("service_status", "COMPLETE");
        refundQw.apply("IFNULL(refund_time, update_time) BETWEEN {0} AND {1}", dates[0], dates[1]);
        refundQw.eq(StringUtils.isNotEmpty(storeId), "store_id", storeId);
        m.refundAmount = nullToZero(orderStatisticsMapper.sumGoodsRefundAmount(refundQw));
        m.refundNum = nullToZero(orderStatisticsMapper.sumGoodsRefundNum(refundQw));

        m.netSalesAmount = CurrencyUtil.sub(m.salesAmount, m.refundAmount);
        m.netSalesNum = m.salesNum - m.refundNum;
        m.discountAmount = calcDiscount(dates, storeId);
        m.salePriceAmount = CurrencyUtil.add(m.netSalesAmount, m.discountAmount);
        return m;
    }

    private Double calcDiscount(Date[] dates, String storeId) {
        // 按下单项关联已支付订单，按支付时间统计优惠（扣除全部退款项）
        QueryWrapper<OrderItem> qw = Wrappers.query();
        if (StringUtils.isNotEmpty(storeId)) {
            qw.apply("EXISTS (SELECT 1 FROM li_order o WHERE o.sn = li_order_item.order_sn AND o.pay_status = {0} AND o.payment_time BETWEEN {1} AND {2} AND o.store_id = {3})",
                    PayStatusEnum.PAID.name(), dates[0], dates[1], storeId);
        } else {
            qw.apply("EXISTS (SELECT 1 FROM li_order o WHERE o.sn = li_order_item.order_sn AND o.pay_status = {0} AND o.payment_time BETWEEN {1} AND {2})",
                    PayStatusEnum.PAID.name(), dates[0], dates[1]);
        }
        qw.ne("is_refund", RefundStatusEnum.ALL_REFUND.name());
        List<OrderItem> items = orderItemService.list(qw);
        double total = 0D;
        for (OrderItem item : items) {
            PriceDetailDTO dto = item.getPriceDetailDTO();
            if (dto == null) {
                continue;
            }
            double discount = CurrencyUtil.add(
                    dto.getDiscountPrice() == null ? 0D : dto.getDiscountPrice(),
                    dto.getCouponPrice() == null ? 0D : dto.getCouponPrice());
            if (RefundStatusEnum.NO_REFUND.name().equals(item.getIsRefund())) {
                total = CurrencyUtil.add(total, discount);
            } else if (item.getNum() != null && item.getNum() > 0 && item.getReturnGoodsNumber() != null) {
                int remain = item.getNum() - item.getReturnGoodsNumber();
                if (remain > 0) {
                    total = CurrencyUtil.add(total, CurrencyUtil.mul(discount, CurrencyUtil.div(remain, item.getNum(), 4)));
                }
            }
        }
        return total;
    }

    @Override
    public List<GoodsRankVO> refundRank(StatisticsQueryParam statisticsQueryParam) {
        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);
        QueryWrapper qw = Wrappers.query();
        qw.eq("service_status", "COMPLETE");
        qw.apply("IFNULL(refund_time, update_time) BETWEEN {0} AND {1}", dates[0], dates[1]);
        qw.eq(StringUtils.isNotEmpty(statisticsQueryParam.getStoreId()), "store_id", statisticsQueryParam.getStoreId());
        qw.groupBy("sku_id");
        qw.last("ORDER BY amount DESC LIMIT 10");
        List<GoodsRankVO> list = orderStatisticsMapper.rankRefundBySku(qw);
        fillRank(list);
        return list;
    }

    @Override
    public List<GoodsRankVO> salesRank(StatisticsQueryParam statisticsQueryParam) {
        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);
        QueryWrapper salesQw = Wrappers.query();
        salesQw.between("o.payment_time", dates[0], dates[1]);
        salesQw.eq("o.pay_status", PayStatusEnum.PAID.name());
        salesQw.eq(StringUtils.isNotEmpty(statisticsQueryParam.getStoreId()), "o.store_id", statisticsQueryParam.getStoreId());
        salesQw.groupBy("oi.sku_id");
        List<GoodsRankVO> sales = orderStatisticsMapper.rankSalesBySku(salesQw);

        QueryWrapper refundQw = Wrappers.query();
        refundQw.eq("service_status", "COMPLETE");
        refundQw.apply("IFNULL(refund_time, update_time) BETWEEN {0} AND {1}", dates[0], dates[1]);
        refundQw.eq(StringUtils.isNotEmpty(statisticsQueryParam.getStoreId()), "store_id", statisticsQueryParam.getStoreId());
        refundQw.groupBy("sku_id");
        List<GoodsRankVO> refunds = orderStatisticsMapper.rankRefundBySku(refundQw);
        Map<String, Double> refundMap = new HashMap<>();
        if (refunds != null) {
            for (GoodsRankVO r : refunds) {
                refundMap.put(r.getSkuId(), r.getAmount() == null ? 0D : r.getAmount());
            }
        }
        if (sales != null) {
            for (GoodsRankVO s : sales) {
                double refund = refundMap.getOrDefault(s.getSkuId(), 0D);
                s.setAmount(CurrencyUtil.sub(s.getAmount() == null ? 0D : s.getAmount(), refund));
            }
            sales.sort((a, b) -> Double.compare(b.getAmount() == null ? 0D : b.getAmount(), a.getAmount() == null ? 0D : a.getAmount()));
            if (sales.size() > 10) {
                sales = sales.subList(0, 10);
            }
            fillRank(sales);
        }
        return sales;
    }

    private void fillRank(List<GoodsRankVO> list) {
        if (list == null) {
            return;
        }
        int i = 1;
        for (GoodsRankVO vo : list) {
            vo.setRank(i++);
        }
    }

    private Double nullToZero(Double v) {
        return v == null ? 0D : v;
    }

    private Long nullToZero(Long v) {
        return v == null ? 0L : v;
    }

    private static class PeriodMetrics {
        Double salesAmount;
        Double refundAmount;
        Double netSalesAmount;
        Double discountAmount;
        Double salePriceAmount;
        Long salesNum;
        Long refundNum;
        Long netSalesNum;
    }
}
