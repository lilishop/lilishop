package cn.lili.modules.statistics.serviceimpl;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.date.DateTime;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.trade.entity.enums.AfterSaleTypeEnum;
import cn.lili.modules.statistics.entity.dto.GoodsStatisticsQueryParam;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.enums.SearchTypeEnum;
import cn.lili.modules.statistics.entity.vo.*;
import cn.lili.modules.statistics.service.*;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import cn.lili.modules.store.entity.enums.BillStatusEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * 首页统计数据业务层实现
 *
 * @author Bulbasaur
 * @since 2020/12/15 17:57
 */
@Service
public class IndexStatisticsServiceImpl implements IndexStatisticsService {

    /**
     * 订单统计
     */
    @Autowired
    private OrderStatisticsService orderStatisticsService;
    /**
     * 会员统计
     */
    @Autowired
    private MemberStatisticsService memberStatisticsService;
    /**
     * 商品统计
     */
    @Autowired
    private GoodsStatisticsService goodsStatisticsService;
    /**
     * 商品统计
     */
    @Autowired
    private StoreFlowStatisticsService storeFlowStatisticsService;
    /**
     * 店铺
     */
    @Autowired
    private StoreStatisticsService storeStatisticsService;
    /**
     * 店铺
     */
    @Autowired
    private MemberEvaluationStatisticsService memberEvaluationStatisticsService;
    /**
     * 售后
     */
    @Autowired
    private AfterSaleStatisticsService afterSaleStatisticsService;
    /**
     * 投诉
     */
    @Autowired
    private OrderComplaintStatisticsService orderComplaintStatisticsService;
    /**
     * 分销员提现
     */
    @Autowired
    private DistributionCashStatisticsService distributionCashStatisticsService;
    /**
     * 平台PV统计
     */
    @Autowired
    private PlatformViewService platformViewService;
    /**
     * 结算单
     */
    @Autowired
    private BillStatisticsService billStatisticsService;
    /**
     * 秒杀活动
     */
    @Autowired
    private SeckillStatisticsService seckillStatisticsService;

    @Override
    public IndexNoticeVO indexNotice() {

        IndexNoticeVO indexNoticeVO = new IndexNoticeVO();
        //商品审核
        indexNoticeVO.setGoods(goodsStatisticsService.goodsNum(null, GoodsAuthEnum.TOBEAUDITED));
        //店铺入驻审核
        indexNoticeVO.setStore(storeStatisticsService.auditNum());
        //售后申请
        indexNoticeVO.setRefund(afterSaleStatisticsService.applyNum(null));
        //投诉审核
        indexNoticeVO.setComplain(orderComplaintStatisticsService.waitComplainNum());
        //分销员提现审核
        indexNoticeVO.setDistributionCash(distributionCashStatisticsService.newDistributionCash());
        //待处理商家结算
        indexNoticeVO.setWaitPayBill(billStatisticsService.billNum(BillStatusEnum.CHECK));
        return indexNoticeVO;
    }

    @Override
    public IndexStatisticsVO indexStatistics() {

        //首页统计内容
        IndexStatisticsVO indexStatisticsVO = new IndexStatisticsVO();

        //获取总订单数量
        indexStatisticsVO.setOrderNum(orderStatisticsService.orderNum(null));
        //获取总会员数量
        indexStatisticsVO.setMemberNum(memberStatisticsService.getMemberCount());
        //获取总上架商品数量
        indexStatisticsVO.setGoodsNum(goodsStatisticsService.goodsNum(GoodsStatusEnum.UPPER, GoodsAuthEnum.PASS));
        //获取总店铺数量
        indexStatisticsVO.setStoreNum(storeStatisticsService.storeNum());

        //下单统计
        Map<String, Object> map = storeFlowStatisticsService.getOrderStatisticsPrice();
        //今日下单数
        indexStatisticsVO.setTodayOrderNum(map.get("num") == null ? 0L : (Long) map.get("num"));
        //今日下单金额
        indexStatisticsVO.setTodayOrderPrice(map.get("price") == null ? 0D : (Double) map.get("price"));

        //今日新增会员数量
        indexStatisticsVO.setTodayMemberNum(memberStatisticsService.todayMemberNum());
        //今日新增商品数量
        indexStatisticsVO.setTodayGoodsNum(goodsStatisticsService.todayUpperNum());
        //今日新增店铺数量
        indexStatisticsVO.setTodayStoreNum(storeStatisticsService.todayStoreNum());
        //今日新增评论数量
        indexStatisticsVO.setTodayMemberEvaluation(memberEvaluationStatisticsService.todayMemberEvaluation());
        //当前在线人数
        indexStatisticsVO.setCurrentNumberPeopleOnline(platformViewService.online());


        //流量统计
        StatisticsQueryParam queryParam = new StatisticsQueryParam();

        //今日uv
        queryParam.setSearchType(SearchTypeEnum.TODAY.name());
        indexStatisticsVO.setTodayUV(platformViewService.countUv(queryParam));

//       昨日访问数UV
        queryParam.setSearchType(SearchTypeEnum.YESTERDAY.name());
        indexStatisticsVO.setYesterdayUV(platformViewService.countUv(queryParam));

//       前七日访问数UV
        queryParam.setSearchType(SearchTypeEnum.LAST_SEVEN.name());
        indexStatisticsVO.setLastSevenUV(platformViewService.countUv(queryParam));

//       三十日访问数UV
        queryParam.setSearchType(SearchTypeEnum.LAST_THIRTY.name());
        indexStatisticsVO.setLastThirtyUV(platformViewService.countUv(queryParam));


        return indexStatisticsVO;
    }

    @Override
    public StoreIndexStatisticsVO storeIndexStatistics() {

        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        StoreIndexStatisticsVO storeIndexStatisticsVO = new StoreIndexStatisticsVO();

        //商品总数量
        storeIndexStatisticsVO.setGoodsNum(goodsStatisticsService.goodsNum(GoodsStatusEnum.UPPER, null));
        //订单总数量、订单总金额
        Map<String, Object> map = storeFlowStatisticsService.getOrderStatisticsPrice();
        storeIndexStatisticsVO.setOrderNum(Convert.toInt(map.get("num").toString()));
        storeIndexStatisticsVO.setOrderPrice(map.get("price") != null ? Double.parseDouble(map.get("price").toString()) : 0.0);

        //访问量
        StatisticsQueryParam queryParam = new StatisticsQueryParam();
        queryParam.setSearchType(SearchTypeEnum.TODAY.name());
        queryParam.setStoreId(storeId);
        PlatformViewVO platformViewVO = platformViewService.list(queryParam).get(0);
        storeIndexStatisticsVO.setStoreUV(platformViewVO.getUvNum().intValue());

        //待付款订单数量
        storeIndexStatisticsVO.setUnPaidOrder(orderStatisticsService.orderNum(OrderStatusEnum.UNPAID.name()));
        //待发货订单数量
        storeIndexStatisticsVO.setUnDeliveredOrder(orderStatisticsService.orderNum(OrderStatusEnum.UNDELIVERED.name()));
        //待收货订单数量
        storeIndexStatisticsVO.setDeliveredOrder(orderStatisticsService.orderNum(OrderStatusEnum.DELIVERED.name()));

        //待处理退货数量
        storeIndexStatisticsVO.setReturnGoods(afterSaleStatisticsService.applyNum(AfterSaleTypeEnum.RETURN_GOODS.name()));
        //待处理退款数量
        storeIndexStatisticsVO.setReturnMoney(afterSaleStatisticsService.applyNum(AfterSaleTypeEnum.RETURN_MONEY.name()));
        //待回复评价数量
        storeIndexStatisticsVO.setMemberEvaluation(memberEvaluationStatisticsService.getWaitReplyNum());
        //待处理投诉数量
        storeIndexStatisticsVO.setComplaint(orderComplaintStatisticsService.waitComplainNum());

        //待上架商品数量
        storeIndexStatisticsVO.setWaitUpper(goodsStatisticsService.goodsNum(GoodsStatusEnum.DOWN, null));
        //待审核商品数量
        storeIndexStatisticsVO.setWaitAuth(goodsStatisticsService.goodsNum(null, GoodsAuthEnum.TOBEAUDITED));

        //可参与秒杀活动数量
        storeIndexStatisticsVO.setSeckillNum(seckillStatisticsService.getApplyNum());
        //待处理商家结算
        storeIndexStatisticsVO.setWaitPayBill(billStatisticsService.billNum(BillStatusEnum.OUT));

        return storeIndexStatisticsVO;
    }

    @Override
    public List<GoodsStatisticsDataVO> goodsStatistics(GoodsStatisticsQueryParam statisticsQueryParam) {
        //查询商品
        return storeFlowStatisticsService.getGoodsStatisticsData(statisticsQueryParam, 10);
    }

    @Override
    public List<StoreStatisticsDataVO> storeStatistics(StatisticsQueryParam statisticsQueryParam) {

        QueryWrapper queryWrapper = Wrappers.query();

        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);
        Date startTime = dates[0], endTime = dates[1];
        queryWrapper.between("create_time", startTime, endTime);

        queryWrapper.orderByDesc("price");

        queryWrapper.groupBy("store_id,store_name ");

        queryWrapper.eq("flow_type", FlowTypeEnum.PAY.name());

        //查询前十条记录
        Page page = new Page<StoreStatisticsDataVO>(1, 10);

        return storeFlowStatisticsService.getStoreStatisticsData(page, queryWrapper);
    }


    /**
     * 获取当月查询参数
     *
     * @return 当月查询参数
     */
    private StatisticsQueryParam getStatisticsQueryParam() {

        StatisticsQueryParam statisticsQueryParam = new StatisticsQueryParam();

        return statisticsQueryParam;
    }

    /**
     * 获取当月订单查询条件
     *
     * @return 当月订单查询参数
     */
    private GoodsStatisticsQueryParam getGoodsStatisticsQueryParam() {
        GoodsStatisticsQueryParam goodsStatisticsQueryParam = new GoodsStatisticsQueryParam();
        StatisticsQueryParam statisticsQueryParam = this.getStatisticsQueryParam();
        BeanUtil.copyProperties(goodsStatisticsQueryParam, statisticsQueryParam);

        //如果登录是商家的账号，获取商家相关统计内容
        if (UserContext.getCurrentUser().getRole().equals(UserEnums.STORE)) {
            goodsStatisticsQueryParam.setStoreId(UserContext.getCurrentUser().getStoreId());
        }
        DateTime dateTime = new DateTime();
        goodsStatisticsQueryParam.setYear(dateTime.year());
        goodsStatisticsQueryParam.setMonth(dateTime.monthBaseOne());
        return goodsStatisticsQueryParam;
    }
}
