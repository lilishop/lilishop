package cn.lili.modules.statistics.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.OrderOverviewVO;
import cn.lili.modules.statistics.entity.vo.OrderStatisticsDataVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

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
}