package cn.lili.modules.statistics.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.enums.RefundStatusEnum;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.OrderOverviewVO;
import cn.lili.modules.statistics.entity.vo.OrderStatisticsDataVO;
import cn.lili.modules.statistics.mapper.OrderStatisticsMapper;
import cn.lili.modules.statistics.service.OrderStatisticsService;
import cn.lili.modules.statistics.service.PlatformViewService;
import cn.lili.modules.statistics.service.StoreFlowStatisticsService;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * 订单统计业务层实现
 *
 * @author Bulbasaur
 * @since 2020/12/9 17:16
 */
@Service
public class OrderStatisticsServiceImpl extends ServiceImpl<OrderStatisticsMapper, Order> implements OrderStatisticsService {



    /**
     * 平台PV统计
     */
    @Autowired
    private PlatformViewService platformViewService;

    @Autowired
    private StoreFlowStatisticsService storeFlowStatisticsService;
    @Autowired
    private OrderItemService orderItemService;

    @Override
    public OrderOverviewVO overview(StatisticsQueryParam statisticsQueryParam) {
        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);

        OrderOverviewVO orderOverviewVO = new OrderOverviewVO();

        /**
         * 组织统计初始化
         */
        storeFlowStatisticsService.overview(dates, orderOverviewVO, statisticsQueryParam);
        //访客数
        Integer uv = platformViewService.countUv(statisticsQueryParam);
        if (uv != null) {
            orderOverviewVO.setUvNum(uv.longValue());
        }

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

    @Override
    public long orderNum(String orderStatus) {
        LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(orderStatus), Order::getOrderStatus, orderStatus);
        queryWrapper.eq(CharSequenceUtil.equals(Objects.requireNonNull(UserContext.getCurrentUser()).getRole().name(), UserEnums.STORE.name()),
                Order::getStoreId, UserContext.getCurrentUser().getStoreId());
        return this.count(queryWrapper);
    }

    @Override
    public long orderNum(String paymentMethod, Date[] dates) {
        LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(paymentMethod), Order::getPaymentMethod, paymentMethod);
        queryWrapper.between(Order::getCreateTime, dates[0], dates[1]);
        return this.count(queryWrapper);
    }


    @Override
    public Double getDiscountPrice(Date[] dates) {
        // 参数校验
        if (dates == null || dates.length < 2) {
            return 0.0;
        }
        
        // 构建查询条件：按时间范围过滤，排除全部退款的订单项
        LambdaQueryWrapper<OrderItem> queryWrapper = new LambdaQueryWrapper<OrderItem>()
                .ne(OrderItem::getIsRefund, RefundStatusEnum.ALL_REFUND.name())
                .between(OrderItem::getCreateTime, dates[0], dates[1]);
        
        List<OrderItem> orderItems = orderItemService.list(queryWrapper);
        
        if (orderItems.isEmpty()) {
            return 0.0;
        }
        
        Double totalDiscountPrice = 0.0;
        
        for (OrderItem orderItem : orderItems) {
            PriceDetailDTO priceDetailDTO = orderItem.getPriceDetailDTO();
            if (priceDetailDTO == null) {
                continue;
            }
            
            Double itemDiscountPrice = calculateItemDiscountPrice(priceDetailDTO);
            
            if (RefundStatusEnum.NO_REFUND.name().equals(orderItem.getIsRefund())) {
                // 未退款：计算全部优惠金额
                totalDiscountPrice = CurrencyUtil.add(totalDiscountPrice, itemDiscountPrice);
            } else {
                // 部分退款：按比例计算剩余优惠金额
                Double remainingDiscountPrice = calculateRemainingDiscountPrice(
                        itemDiscountPrice, orderItem.getNum(), orderItem.getReturnGoodsNumber());
                totalDiscountPrice = CurrencyUtil.add(totalDiscountPrice, remainingDiscountPrice);
            }
        }
        
        return totalDiscountPrice;
    }

    @Override
    public long getPayOrderNum(Date[] dates) {

        LambdaQueryWrapper<OrderItem> orderItemLambdaQueryWrapper=new LambdaQueryWrapper<>();

        orderItemLambdaQueryWrapper.between(OrderItem::getCreateTime,dates[0], dates[1]);
        orderItemLambdaQueryWrapper.ne(OrderItem::getIsRefund,RefundStatusEnum.ALL_REFUND.name());

        return this.baseMapper.getPayOrderNum(orderItemLambdaQueryWrapper);
    }

    @Override
    public Double getPayOrderPrice(Date[] dates, PaymentMethodEnum paymentMethodEnum, DeliveryMethodEnum deliveryMethodEnum) {

        //查看付款金额
        QueryWrapper queryWrapper = Wrappers.query();

        queryWrapper.between("oi.create_time", dates[0], dates[1]);
        queryWrapper.ne("oi.is_refund", RefundStatusEnum.ALL_REFUND.name());

        if(Objects.nonNull(paymentMethodEnum)){
            queryWrapper.eq("o.payment_method",paymentMethodEnum.name());
        }
        if(Objects.nonNull(deliveryMethodEnum)){
            if(DeliveryMethodEnum.VIRTUAL.equals(deliveryMethodEnum)){
                queryWrapper.eq("o.order_type", OrderTypeEnum.VIRTUAL.name());
            }else{
                queryWrapper.eq("o.delivery_method",deliveryMethodEnum.name());
            }

        }
        return this.baseMapper.getPayOrderPrice(queryWrapper);

    }

    @Override
    public Double getGoodsPrice(Date[] dates) {
        LambdaQueryWrapper<OrderItem> orderItemLambdaQueryWrapper=new LambdaQueryWrapper<>();

        orderItemLambdaQueryWrapper.between(OrderItem::getCreateTime,dates[0], dates[1]);
        orderItemLambdaQueryWrapper.ne(OrderItem::getIsRefund,RefundStatusEnum.ALL_REFUND.name());

        return this.baseMapper.getGoodsPrice(orderItemLambdaQueryWrapper);
    }

    @Override
    public Double getFreight(Date[] dates) {

        LambdaQueryWrapper<OrderItem> orderItemLambdaQueryWrapper=new LambdaQueryWrapper<>();

        orderItemLambdaQueryWrapper.between(OrderItem::getCreateTime,dates[0], dates[1]);
        orderItemLambdaQueryWrapper.ne(OrderItem::getIsRefund,RefundStatusEnum.ALL_REFUND.name());

        List<OrderItem> orderItems=orderItemService.list(orderItemLambdaQueryWrapper);
        Double freight=0D;
        for (OrderItem orderItem:orderItems){
            PriceDetailDTO priceDetailDTO=orderItem.getPriceDetailDTO();
            freight=CurrencyUtil.add(freight,priceDetailDTO.getFreightPrice());
        }
        return freight;
    }

    @Override
    public Double getDistribution(Date[] dates) {
        LambdaQueryWrapper<OrderItem> orderItemLambdaQueryWrapper=new LambdaQueryWrapper<>();

        orderItemLambdaQueryWrapper.between(OrderItem::getCreateTime,dates[0], dates[1]);
        orderItemLambdaQueryWrapper.ne(OrderItem::getIsRefund,RefundStatusEnum.ALL_REFUND.name());

        List<OrderItem> orderItems=orderItemService.list(orderItemLambdaQueryWrapper);
        Double distributionCommission=0D;
        for (OrderItem orderItem:orderItems){
            PriceDetailDTO priceDetailDTO=orderItem.getPriceDetailDTO();
            distributionCommission=CurrencyUtil.add(distributionCommission,priceDetailDTO.getDistributionCommission());
        }
        return distributionCommission;
    }

    @Override
    public Long getRefundNum(Date[] dates) {
        LambdaQueryWrapper<OrderItem> orderItemLambdaQueryWrapper=new LambdaQueryWrapper<>();

        orderItemLambdaQueryWrapper.between(OrderItem::getCreateTime,dates[0], dates[1]);
        orderItemLambdaQueryWrapper.eq(OrderItem::getIsRefund,RefundStatusEnum.ALL_REFUND.name());

        return this.baseMapper.getPayOrderNum(orderItemLambdaQueryWrapper);
    }

    @Override
    public Double getRefundPrice(Date[] dates) {

        QueryWrapper queryWrapper = Wrappers.query();

        queryWrapper.between("oi.create_time", dates[0], dates[1]);
        queryWrapper.eq("oi.is_refund", RefundStatusEnum.ALL_REFUND.name());


        return this.baseMapper.getRefundPrice(queryWrapper);
    }

    @Override
    public Double getRefundRate(Date[] dates) {

        QueryWrapper queryWrapper = Wrappers.query();

        queryWrapper.between("create_time", dates[0], dates[1]);


        Long orderNum= this.baseMapper.getPayOrderNum(queryWrapper);
        return CurrencyUtil.mul(CurrencyUtil.div(this.getRefundNum(dates),orderNum),100);
    }

    /**
     * 计算订单项的优惠金额
     */
    private Double calculateItemDiscountPrice(PriceDetailDTO priceDetailDTO) {
        Double discountPrice = priceDetailDTO.getDiscountPrice() != null ? priceDetailDTO.getDiscountPrice() : 0.0;
        Double couponPrice = priceDetailDTO.getCouponPrice() != null ? priceDetailDTO.getCouponPrice() : 0.0;
        return CurrencyUtil.add(discountPrice, couponPrice);
    }

    /**
     * 计算部分退款后的剩余优惠金额
     */
    private Double calculateRemainingDiscountPrice(Double totalDiscountPrice, Integer totalNum, Integer returnNum) {
        if (totalNum == null || totalNum <= 0 || returnNum == null || returnNum < 0) {
            return totalDiscountPrice;
        }
        
        Integer remainingNum = totalNum - returnNum;
        if (remainingNum <= 0) {
            return 0.0;
        }
        
        // 按剩余数量比例计算优惠金额
        Double ratio = CurrencyUtil.div(remainingNum.doubleValue(), totalNum.doubleValue(), 4);
        return CurrencyUtil.mul(totalDiscountPrice, ratio);
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

    @Override
    public IPage<OrderSimpleVO> getStatistics(StatisticsQueryParam statisticsQueryParam, PageVO pageVO) {

        QueryWrapper<OrderSimpleVO> queryWrapper = new QueryWrapper<>();
        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);
        queryWrapper.between("o.create_time", dates[0], dates[1]);
        queryWrapper.eq(StringUtils.isNotEmpty(statisticsQueryParam.getStoreId()),
                "o.store_id", statisticsQueryParam.getStoreId());

        queryWrapper.eq("o.delete_flag", false);
        queryWrapper.groupBy("o.id");
        queryWrapper.orderByDesc("o.id");
        return this.baseMapper.queryByParams(PageUtil.initPage(pageVO), queryWrapper);
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
