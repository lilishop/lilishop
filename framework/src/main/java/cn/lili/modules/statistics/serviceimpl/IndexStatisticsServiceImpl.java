package cn.lili.modules.statistics.serviceimpl;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.date.DateTime;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.distribution.service.DistributionCashService;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.member.service.MemberEvaluationService;
import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.service.AfterSaleService;
import cn.lili.modules.order.order.service.OrderComplaintService;
import cn.lili.modules.order.trade.entity.enums.AfterSaleTypeEnum;
import cn.lili.modules.promotion.service.SeckillService;
import cn.lili.modules.statistics.mapper.StoreStatisticsDataMapper;
import cn.lili.modules.statistics.model.dto.GoodsStatisticsQueryParam;
import cn.lili.modules.statistics.model.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.model.enums.SearchTypeEnum;
import cn.lili.modules.statistics.model.vo.*;
import cn.lili.modules.statistics.service.*;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import cn.lili.modules.store.entity.enums.BillStatusEnum;
import cn.lili.modules.store.service.BillService;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 首页统计数据业务层实现
 *
 * @author Bulbasaur
 * @date 2020/12/15 17:57
 */
@Service
public class IndexStatisticsServiceImpl implements IndexStatisticsService {

    /**
     * 订单统计
     */
    @Autowired
    private OrderStatisticsDataService orderStatisticsDataService;
    /**
     * 会员统计
     */
    @Autowired
    private MemberStatisticsDataService memberStatisticsDataService;
    /**
     * 商品统计
     */
    @Autowired
    private GoodsStatisticsDataService goodsStatisticsDataService;
    /**
     * 店铺统计
     */
    @Autowired
    private StoreStatisticsDataMapper storeStatisticsDataMapper;
    /**
     * 商品
     */
    @Autowired
    private GoodsService goodsService;
    /**
     * 店铺
     */
    @Autowired
    private StoreService storeService;
    /**
     * 店铺
     */
    @Autowired
    private MemberEvaluationService memberEvaluationService;
    /**
     * 售后
     */
    @Autowired
    private AfterSaleService afterSaleService;
    /**
     * 投诉
     */
    @Autowired
    private OrderComplaintService orderComplaintService;
    /**
     * 分销员提现
     */
    @Autowired
    private DistributionCashService distributionCashService;
    /**
     * 平台PV统计
     */
    @Autowired
    private PlatformViewDataService platformViewDataService;
    /**
     * 结算单
     */
    @Autowired
    private BillService billService;
    /**
     * 秒杀活动
     */
    @Autowired
    private SeckillService seckillService;

    @Override
    public IndexNoticeVO indexNotice() {

        IndexNoticeVO indexNoticeVO = new IndexNoticeVO();
        //商品审核
        indexNoticeVO.setGoods(goodsService.goodsNum(null, GoodsAuthEnum.TOBEAUDITED));
        //店铺入驻审核
        indexNoticeVO.setStore(storeService.auditNum());
        //售后申请
        indexNoticeVO.setRefund(afterSaleService.applyNum(null));
        //投诉审核
        indexNoticeVO.setComplain(orderComplaintService.waitComplainNum());
        //分销员提现审核
        indexNoticeVO.setDistributionCash(distributionCashService.newDistributionCash());
        //待处理商家结算
        indexNoticeVO.setWaitPayBill(billService.billNum(BillStatusEnum.CHECK));
        return indexNoticeVO;
    }

    @Override
    public IndexStatisticsVO indexStatistics() {

        //首页统计内容
        IndexStatisticsVO indexStatisticsVO = new IndexStatisticsVO();

        //获取总订单数量
        indexStatisticsVO.setOrderNum(orderStatisticsDataService.orderNum(null));
        //获取总会员数量
        indexStatisticsVO.setMemberNum(memberStatisticsDataService.getMemberCount());
        //获取总上架商品数量
        indexStatisticsVO.setGoodsNum(goodsService.goodsNum(GoodsStatusEnum.UPPER, GoodsAuthEnum.PASS));
        //获取总店铺数量
        indexStatisticsVO.setStoreNum(storeService.storeNum());

        //下单统计
        Map<String, Object> map = orderStatisticsDataService.getOrderStatisticsPrice();
        //今日下单数
        indexStatisticsVO.setTodayOrderNum(map.get("num") == null ? 0L : (Long) map.get("num"));
        //今日下单金额
        indexStatisticsVO.setTodayOrderPrice(map.get("price") == null ? 0D : (Double) map.get("price"));

        //今日新增会员数量
        indexStatisticsVO.setTodayMemberNum(memberStatisticsDataService.todayMemberNum());
        //今日新增商品数量
        indexStatisticsVO.setTodayGoodsNum(goodsService.todayUpperNum());
        //今日新增店铺数量
        indexStatisticsVO.setTodayStoreNum(storeService.todayStoreNum());
        //今日新增评论数量
        indexStatisticsVO.setTodayMemberEvaluation(memberEvaluationService.todayMemberEvaluation());
        //当前在线人数
        indexStatisticsVO.setCurrentNumberPeopleOnline(platformViewDataService.online());


        //流量统计
        StatisticsQueryParam queryParam = new StatisticsQueryParam();

        //今日uv
        queryParam.setSearchType(SearchTypeEnum.TODAY.name());
        indexStatisticsVO.setTodayUV(platformViewDataService.countUv(queryParam));

//       昨日访问数UV
        queryParam.setSearchType(SearchTypeEnum.YESTERDAY.name());
        indexStatisticsVO.setYesterdayUV(platformViewDataService.countUv(queryParam));

//       前七日访问数UV
        queryParam.setSearchType(SearchTypeEnum.LAST_SEVEN.name());
        indexStatisticsVO.setLastSevenUV(platformViewDataService.countUv(queryParam));

//       三十日访问数UV
        queryParam.setSearchType(SearchTypeEnum.LAST_THIRTY.name());
        indexStatisticsVO.setLastThirtyUV(platformViewDataService.countUv(queryParam));


        return indexStatisticsVO;
    }

    @Override
    public StoreIndexStatisticsVO storeIndexStatistics() {

        StoreIndexStatisticsVO storeIndexStatisticsVO = new StoreIndexStatisticsVO();

        //商品总数量
        storeIndexStatisticsVO.setGoodsNum(goodsService.goodsNum(GoodsStatusEnum.UPPER, null));
        //订单总数量、订单总金额
        Map<String, Object> map = orderStatisticsDataService.getStoreOrderStatisticsPrice();
        storeIndexStatisticsVO.setOrderNum(Convert.toInt(map.get("num").toString()));
        storeIndexStatisticsVO.setOrderPrice(map.get("price") != null ? Double.parseDouble(map.get("price").toString()) : 0.0);

        //访问量
        StatisticsQueryParam queryParam = new StatisticsQueryParam();
        queryParam.setSearchType(SearchTypeEnum.TODAY.name());
        queryParam.setStoreId(UserContext.getCurrentUser().getStoreId());
        PlatformViewVO platformViewVO = platformViewDataService.list(queryParam).get(0);
        storeIndexStatisticsVO.setStoreUV(platformViewVO.getUvNum().intValue());

        //待付款订单数量
        storeIndexStatisticsVO.setUnPaidOrder(orderStatisticsDataService.orderNum(OrderStatusEnum.UNPAID.name()));
        //待发货订单数量
        storeIndexStatisticsVO.setUnDeliveredOrder(orderStatisticsDataService.orderNum(OrderStatusEnum.UNDELIVERED.name()));
        //待收货订单数量
        storeIndexStatisticsVO.setDeliveredOrder(orderStatisticsDataService.orderNum(OrderStatusEnum.DELIVERED.name()));

        //待处理退货数量
        storeIndexStatisticsVO.setReturnGoods(afterSaleService.applyNum(AfterSaleTypeEnum.RETURN_GOODS.name()));
        //待处理退款数量
        storeIndexStatisticsVO.setReturnMoney(afterSaleService.applyNum(AfterSaleTypeEnum.RETURN_MONEY.name()));
        //待回复评价数量
        storeIndexStatisticsVO.setMemberEvaluation(memberEvaluationService.getWaitReplyNum());
        //待处理投诉数量
        storeIndexStatisticsVO.setComplaint(orderComplaintService.waitComplainNum());

        //待上架商品数量
        storeIndexStatisticsVO.setWaitUpper(goodsService.goodsNum(GoodsStatusEnum.DOWN, null));
        //待审核商品数量
        storeIndexStatisticsVO.setWaitAuth(goodsService.goodsNum(null, GoodsAuthEnum.TOBEAUDITED));

        //可参与秒杀活动数量
        storeIndexStatisticsVO.setSeckillNum(seckillService.getApplyNum());
        //待处理商家结算
        storeIndexStatisticsVO.setWaitPayBill(billService.billNum(BillStatusEnum.OUT));

        return storeIndexStatisticsVO;
    }

    @Override
    public List<GoodsStatisticsDataVO> goodsStatistics(GoodsStatisticsQueryParam statisticsQueryParam) {
        //查询商品
        return goodsStatisticsDataService.getGoodsStatisticsData(statisticsQueryParam, 10);
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

        return storeStatisticsDataMapper.getStoreStatisticsData(page, queryWrapper);
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
