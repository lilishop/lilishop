package cn.lili.modules.statistics.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.OrderOverviewVO;
import cn.lili.modules.statistics.entity.vo.OrderStatisticsDataVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.Date;
import java.util.List;

/**
 * 订单统计业务层
 *
 * @author Bulbasaur
 * @since 2020/12/9 11:06
 */
public interface OrderStatisticsService extends IService<Order> {

    /**
     * 订单统计概览
     *
     * @param statisticsQueryParam
     * @return
     */
    OrderOverviewVO overview(StatisticsQueryParam statisticsQueryParam);

    /**
     * 获取订单总数量
     *
     * @param orderStatus 订单状态
     * @return 订单总数量
     */
    long orderNum(String orderStatus);
    /**
     * 获取订单总数量
     *
     * @param paymentMethod 支付方式
     * @param dates  时间
     *
     * @return 订单总数量
     */
    long orderNum(String paymentMethod, Date[] dates);

    /**
     * 获取所有优惠金额去除退款金额
     * @param dates
     * @return
     */
    Double getDiscountPrice(Date[] dates);

    /**
     * 图表统计
     *
     * @param statisticsQueryParam 统计查询参数
     * @return 订单总数量
     */
    List<OrderStatisticsDataVO> statisticsChart(StatisticsQueryParam statisticsQueryParam);

    /**
     * 获取统计的订单
     *
     * @param statisticsQueryParam
     * @param pageVO
     * @return
     */
    IPage<OrderSimpleVO> getStatistics(StatisticsQueryParam statisticsQueryParam, PageVO pageVO);

    /**
     * 获取付款订单数量 不含全部退款
     *
     * @param dates 时间
     * @return 付款订单数量
     */
    long getPayOrderNum(Date[] dates);

    /**
     * 获取付款订单金额去除退款金额
     *
     * @param dates 时间
     * @param paymentMethodEnum 支付方式
     * @param deliveryMethodEnum 配送方式
     * @return 付款订单金额
     */
    Double getPayOrderPrice(Date[] dates, PaymentMethodEnum paymentMethodEnum, DeliveryMethodEnum deliveryMethodEnum);

    /**
     * 获取商品价格
     * @param dates 时间
     * @return
     */
    Double getGoodsPrice(Date[] dates);

    /**
     * 获取运费
     * @param dates 时间
     * @return
     */
    Double getFreight(Date[] dates);

    /**
     * 获取分销返佣
     * @param dates
     * @return
     */
    Double getDistribution(Date[] dates);

    /**
     * 获取退款订单数
     * @param dates
     * @return
     */
    Long getRefundNum(Date[] dates);

    /**
     * 获取退款金额
     * @param dates
     * @return
     */
    Double getRefundPrice(Date[] dates);

    /**
     * 获取退款率
     * @param dates
     * @return
     */
    Double getRefundRate(Date[] dates);
}