package cn.lili.modules.statistics.serviceimpl;

import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.statistics.entity.dto.GoodsStatisticsQueryParam;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.enums.StatisticsQuery;
import cn.lili.modules.statistics.entity.vo.CategoryStatisticsDataVO;
import cn.lili.modules.statistics.entity.vo.GoodsStatisticsDataVO;
import cn.lili.modules.statistics.entity.vo.OrderOverviewVO;
import cn.lili.modules.statistics.entity.vo.StoreStatisticsDataVO;
import cn.lili.modules.statistics.mapper.StoreFlowStatisticsMapper;
import cn.lili.modules.statistics.service.OrderStatisticsService;
import cn.lili.modules.statistics.service.StoreFlowStatisticsService;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 商品统计业务层实现
 *
 * @author Bulbasaur
 * @since 2020/12/9 11:30
 */
@Service
public class StoreFlowStatisticsServiceImpl extends ServiceImpl<StoreFlowStatisticsMapper, StoreFlow> implements StoreFlowStatisticsService {


    @Autowired
    private OrderStatisticsService orderStatisticsService;

    @Override
    public List<GoodsStatisticsDataVO> getGoodsStatisticsData(GoodsStatisticsQueryParam goodsStatisticsQueryParam, Integer num) {
        //获取查询条件
        QueryWrapper queryWrapper = getQueryWrapper(goodsStatisticsQueryParam);
        //根据商品分组
        queryWrapper.groupBy("goods_id");
        queryWrapper.groupBy("goods_name");

        queryWrapper.eq(!StringUtils.isEmpty(goodsStatisticsQueryParam.getStoreId()), "store_id", goodsStatisticsQueryParam.getStoreId());
        //查询前X记录
        Page page = new Page<GoodsStatisticsDataVO>(1, num);
        return this.baseMapper.getGoodsStatisticsData(page, queryWrapper);
    }

    @Override
    public List<CategoryStatisticsDataVO> getCategoryStatisticsData(GoodsStatisticsQueryParam goodsStatisticsQueryParam) {
        //获取查询条件
        QueryWrapper queryWrapper = getQueryWrapper(goodsStatisticsQueryParam);
        //根据分类分组
        queryWrapper.groupBy("category_id");
        return this.baseMapper.getCateGoryStatisticsData(queryWrapper);
    }

    @Override
    public List<StoreStatisticsDataVO> getStoreStatisticsData(Page page, QueryWrapper queryWrapper) {
        return this.baseMapper.getStoreStatisticsData(page, queryWrapper);
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
        queryWrapper.ge("create_time", cn.lili.common.utils.DateUtil.startOfTodDayTime());

        queryWrapper.select("SUM(final_price) AS price , COUNT(0) AS num");
        return this.getMap(queryWrapper);
    }


    @Override
    public void overview(Date[] dates, OrderOverviewVO orderOverviewVO, StatisticsQueryParam statisticsQueryParam) {
        //下单统计
        initOrder(dates, orderOverviewVO, statisticsQueryParam);

        //付款统计
        initPayment(dates, orderOverviewVO, statisticsQueryParam);

        //退单统计
        initAfterSale(dates, orderOverviewVO, statisticsQueryParam);
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
        Map order = orderStatisticsService.getMap(queryWrapper);
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
        Map memberNum = this.getMap(queryWrapper);
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


        //如果有店铺id传入，则查询店铺
        if (StringUtils.isNotEmpty(statisticsQueryParam.getStoreId())) {
            orderOverviewVO.setPaymentsNum(baseMapper.countPayersByStore(statisticsQueryParam.getStoreId(), dates[0], dates[1]));
        } else {
            orderOverviewVO.setPaymentsNum(baseMapper.countPayers(dates[0], dates[1]));
        }
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


    /**
     * 组织查询条件
     *
     * @param goodsStatisticsQueryParam
     * @return
     */
    private QueryWrapper getQueryWrapper(GoodsStatisticsQueryParam goodsStatisticsQueryParam) {

        QueryWrapper queryWrapper = Wrappers.query();
        //判断搜索类型是：年、月
        Date[] date = StatisticsDateUtil.getDateArray(goodsStatisticsQueryParam);
        queryWrapper.between("create_time", date[0], date[1]);

        //判断是按照数量统计还是按照金额统计
        if (goodsStatisticsQueryParam.getType().equals(StatisticsQuery.PRICE.name())) {
            queryWrapper.orderByDesc("price");
        } else {
            queryWrapper.orderByDesc("num");
        }
        //设置为付款查询
        queryWrapper.eq("flow_type", FlowTypeEnum.PAY.name());
        return queryWrapper;
    }

}
