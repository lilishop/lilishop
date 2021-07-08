package cn.lili.modules.statistics.service;

import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.statistics.model.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.model.vo.OrderOverviewVO;
import cn.lili.modules.statistics.model.vo.OrderStatisticsDataVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;
import java.util.Map;

/**
 * 订单统计业务层
 *
 * @author Bulbasaur
 * @date 2020/12/9 11:06
 */
public interface OrderStatisticsDataService extends IService<StoreFlow> {

    /**
     * 订单统计概览
     *
     * @param statisticsQueryParam
     * @return
     */
    OrderOverviewVO overview(StatisticsQueryParam statisticsQueryParam);

    /**
     * 查询订单统计金额
     *
     * @return 订单统计
     */
    Map<String, Object> getStoreOrderStatisticsPrice();


    /**
     * 查询今日付款统计
     *
     * @return 订单统计金额
     */
    Map<String, Object> getOrderStatisticsPrice();

    /**
     * 获取订单总数量
     *
     * @param orderStatus 订单状态
     * @return 订单总数量
     */
    Integer orderNum(String orderStatus);

    /**
     * 图表统计
     *
     * @param statisticsQueryParam 统计查询参数
     * @return 订单总数量
     */
    List<OrderStatisticsDataVO> statisticsChart(StatisticsQueryParam statisticsQueryParam);

}