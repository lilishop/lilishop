package cn.lili.modules.statistics.serviceimpl;

import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.statistics.mapper.OrderStatisticsDataMapper;
import cn.lili.modules.statistics.model.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.model.vo.OrderOverviewVO;
import cn.lili.modules.statistics.model.vo.OrderStatisticsDataVO;
import cn.lili.modules.statistics.service.OrderStatisticsDataService;
import cn.lili.modules.statistics.service.PlatformViewDataService;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * 订单统计业务层实现
 *
 * @author Bulbasaur
 * @date 2020/12/9 17:16
 */
@Service
public class OrderStatisticsDataServiceImpl extends ServiceImpl<OrderStatisticsDataMapper, StoreFlow> implements OrderStatisticsDataService {
    /**
     * 平台PV统计
     */
    @Autowired
    private PlatformViewDataService platformViewDataService;
    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;

    @Override
    public OrderOverviewVO overview(StatisticsQueryParam statisticsQueryParam) {
        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);

        OrderOverviewVO orderOverviewVO = new OrderOverviewVO();
        //访客数
        orderOverviewVO.setUvNum(platformViewDataService.countUv(statisticsQueryParam));

        //下单统计
        initOrder(dates, orderOverviewVO, statisticsQueryParam);

        //付款统计
        initPayment(dates, orderOverviewVO, statisticsQueryParam);

        //退单统计
        initAfterSale(dates, orderOverviewVO, statisticsQueryParam);

        //数据运算（转换率，比例相关）
        conversionRateOperation(orderOverviewVO);
        return orderOverviewVO;
    }

    /**
     * 运算转换率
     *
     * @param orderOverviewVO 订单统计视图
     */
    private void conversionRateOperation(OrderOverviewVO orderOverviewVO) {

        //下单转换率 订单数/UV
        Double orderConversionRate = CurrencyUtil.div(orderOverviewVO.getOrderNum(), orderOverviewVO.getUvNum(), 4);
        if (orderConversionRate > 1) {
            orderConversionRate = 1d;
        }
        orderOverviewVO.setOrderConversionRate(CurrencyUtil.mul(orderConversionRate, 100) + "%");
        //付款转换率 付款订单数/订单数
        Double paymentsConversionRate = CurrencyUtil.div(orderOverviewVO.getPaymentOrderNum(), orderOverviewVO.getOrderNum(), 4);
        if (paymentsConversionRate > 1) {
            paymentsConversionRate = 1d;
        }
        orderOverviewVO.setPaymentsConversionRate(CurrencyUtil.mul(paymentsConversionRate, 100) + "%");
        //整体转换率 付款数/UV
        Double overallConversionRate = CurrencyUtil.div(orderOverviewVO.getPaymentOrderNum(), orderOverviewVO.getUvNum(), 4);
        if (overallConversionRate > 1) {
            overallConversionRate = 1d;
        }
        orderOverviewVO.setOverallConversionRate(CurrencyUtil.mul(overallConversionRate, 100) + "%");
    }

    /**
     * 订单统计-下单属性填充
     *
     * @param dates
     * @param orderOverviewVO
     */
    private void initOrder(Date[] dates, OrderOverviewVO orderOverviewVO, StatisticsQueryParam statisticsQueryParam) {
        //构建查询条件
        QueryWrapper queryWrapper = Wrappers.query();
        //时间区间
        queryWrapper.between("create_time", dates[0], dates[1]);
        //如果有店铺id传入，则查询店铺
        if (StringUtils.isNotEmpty(statisticsQueryParam.getStoreId())) {
            queryWrapper.eq("store_id", statisticsQueryParam.getStoreId());
        }
        //查询流水金额和订单数量
        queryWrapper.select("SUM(flow_price) AS price , COUNT(0) AS num");
        //获取查询结果
        Map order = orderService.getMap(queryWrapper);
        //赋予订单数和流水金额
        orderOverviewVO.setOrderNum(order != null && order.containsKey("num") ? (Long) order.get("num") : 0L);
        orderOverviewVO.setOrderAmount(order != null && order.containsKey("price") ? (double) order.get("price") : 0L);

        //查询下单人数
        queryWrapper = Wrappers.query();
        //时间区间
        queryWrapper.between("create_time", dates[0], dates[1]);
        //如果有店铺id传入，则查询店铺
        if (StringUtils.isNotEmpty(statisticsQueryParam.getStoreId())) {
            queryWrapper.eq("store_id", statisticsQueryParam.getStoreId());
        }
        //查询下单人数的sql
        queryWrapper.select("count(DISTINCT member_id) AS num");
        //获取查询结果
        Map memberNum = orderService.getMap(queryWrapper);
        //写入下单人数
        orderOverviewVO.setOrderMemberNum(memberNum != null && memberNum.containsKey("num") ? (Long) memberNum.get("num") : 0L);
    }

    /**
     * 订单统计-付款属性填充
     *
     * @param dates
     * @param orderOverviewVO
     */
    private void initPayment(Date[] dates, OrderOverviewVO orderOverviewVO, StatisticsQueryParam statisticsQueryParam) {
        //付款订单数，付款金额
        QueryWrapper queryWrapper = Wrappers.query();
        queryWrapper.between("create_time", dates[0], dates[1]);
        //如果有店铺id传入，则查询店铺
        if (StringUtils.isNotEmpty(statisticsQueryParam.getStoreId())) {
            queryWrapper.eq("store_id", statisticsQueryParam.getStoreId());
        }
        queryWrapper.select("SUM(final_price) AS price , COUNT(0) AS num");
        queryWrapper.eq("flow_type", FlowTypeEnum.PAY.name());
        Map payment = this.getMap(queryWrapper);

        orderOverviewVO.setPaymentOrderNum(payment != null && payment.containsKey("num") ? (Long) payment.get("num") : 0L);
        orderOverviewVO.setPaymentAmount(payment != null && payment.containsKey("price") ? (Double) payment.get("price") : 0D);

        //付款人数
        queryWrapper = Wrappers.query();
        queryWrapper.between("create_time", dates[0], dates[1]);
        //如果有店铺id传入，则查询店铺
        if (StringUtils.isNotEmpty(statisticsQueryParam.getStoreId())) {
            queryWrapper.eq("store_id", statisticsQueryParam.getStoreId());
        }
        queryWrapper.select("COUNT(0) AS num");
        queryWrapper.groupBy("member_id");
        Map paymentMemberNum = this.getMap(queryWrapper);

        orderOverviewVO.setPaymentsNum(paymentMemberNum != null && paymentMemberNum.containsKey("num") ? (Long) paymentMemberNum.get("num") : 0L);
    }

    /**
     * 订单统计-付款属性填充
     *
     * @param dates
     * @param orderOverviewVO
     */
    private void initAfterSale(Date[] dates, OrderOverviewVO orderOverviewVO, StatisticsQueryParam statisticsQueryParam) {
        //付款订单数，付款金额
        QueryWrapper queryWrapper = Wrappers.query();
        queryWrapper.between("create_time", dates[0], dates[1]);
        queryWrapper.select("SUM(final_price) AS price , COUNT(0) AS num");
        //如果有店铺id传入，则查询店铺
        if (StringUtils.isNotEmpty(statisticsQueryParam.getStoreId())) {
            queryWrapper.eq("store_id", statisticsQueryParam.getStoreId());
        }
        queryWrapper.eq("flow_type", FlowTypeEnum.REFUND.name());
        Map payment = this.getMap(queryWrapper);
        orderOverviewVO.setRefundOrderNum(payment != null && payment.containsKey("num") ? (Long) payment.get("num") : 0L);
        orderOverviewVO.setRefundOrderPrice(payment != null && payment.containsKey("price") ? (Double) payment.get("price") : 0D);
    }


    @Override
    public Map<String, Object> getStoreOrderStatisticsPrice() {
        QueryWrapper queryWrapper = new QueryWrapper();
        queryWrapper.eq(StringUtils.equals(UserContext.getCurrentUser().getRole().name(), UserEnums.STORE.name()),
                "store_id", UserContext.getCurrentUser().getStoreId());
        queryWrapper.select("SUM(final_price) AS price , COUNT(0) AS num");
        return this.getMap(queryWrapper);
    }

    @Override
    public Map<String, Object> getOrderStatisticsPrice() {
        QueryWrapper queryWrapper = Wrappers.query();
        //支付订单
        queryWrapper.eq("flow_type", FlowTypeEnum.PAY.name());

        //商家查询，则增加商家判定
        AuthUser authUser = UserContext.getCurrentUser();
        if (authUser.getRole().equals(UserEnums.STORE)) {
            queryWrapper.eq("store_id", authUser.getStoreId());
        }
        //大于今天凌晨
        queryWrapper.gt("create_time", DateUtil.startOfTodDayTime());

        queryWrapper.select("SUM(final_price) AS price , COUNT(0) AS num");
        return this.getMap(queryWrapper);
    }

    @Override
    public Integer orderNum(String orderStatus) {
        LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper();
        queryWrapper.eq(StringUtils.isNotEmpty(orderStatus), Order::getOrderStatus, orderStatus);
        queryWrapper.eq(StringUtils.equals(UserContext.getCurrentUser().getRole().name(), UserEnums.STORE.name()),
                Order::getStoreId, UserContext.getCurrentUser().getStoreId());
        return orderService.count(queryWrapper);
    }

    @Override
    public List<OrderStatisticsDataVO> statisticsChart(StatisticsQueryParam statisticsQueryParam) {
        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);
        QueryWrapper queryWrapper = new QueryWrapper();
        //已支付
        queryWrapper.eq("pay_status", PayStatusEnum.PAID.name());
        //选择商家判定
        queryWrapper.eq(StringUtils.isNotEmpty(statisticsQueryParam.getStoreId()), "store_id", statisticsQueryParam.getStoreId());
//      查询时间区间
        queryWrapper.between("create_time", dates[0], dates[1]);
//       格式化时间
        queryWrapper.groupBy("DATE_FORMAT(create_time,'%Y-%m-%d')");
        List<OrderStatisticsDataVO> orderStatisticsDataVOS = this.baseMapper.getOrderStatisticsData(queryWrapper);
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(dates[0]);

        List<OrderStatisticsDataVO> result = new ArrayList<>();
        //时间判定，将数据填充好
        //如果当前的时间，在结束时间之前
        while (calendar.getTime().before(dates[1])) {
            OrderStatisticsDataVO item = null;
            //判定是否已经有这一天的数据
            for (OrderStatisticsDataVO orderStatisticsDataVO : orderStatisticsDataVOS) {
                if (orderStatisticsDataVO.getCreateTime().equals(calendar.getTime())) {
                    item = orderStatisticsDataVO;
                }
            }
            //如果数据不存在，则进行数据填充
            if (item == null) {
                item = new OrderStatisticsDataVO();
                item.setPrice(0d);
                item.setCreateTime(calendar.getTime());
            }
            result.add(item);
            //增加时间
            calendar.set(Calendar.DAY_OF_MONTH, calendar.get(Calendar.DAY_OF_MONTH) + 1);
        }
        return result;
    }

    private QueryWrapper getQueryWrapper(StatisticsQueryParam statisticsQueryParam) {

        QueryWrapper queryWrapper = Wrappers.query();

        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);
        queryWrapper.between("create_time", dates[0], dates[1]);

        //设置店铺ID
        queryWrapper.eq(StringUtils.isNotEmpty(statisticsQueryParam.getStoreId()), "store_id", statisticsQueryParam.getStoreId());


        //设置为付款查询
        queryWrapper.eq("flow_type", FlowTypeEnum.PAY.name());

        return queryWrapper;
    }

}
