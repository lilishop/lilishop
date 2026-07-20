package cn.lili.modules.statistics.service;

import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.GoodsOverviewVO;
import cn.lili.modules.statistics.entity.vo.GoodsRankVO;

import java.util.List;

/**
 * 商品概况统计业务层
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
public interface GoodsOverviewStatisticsService {

    /**
     * 商品概况指标
     *
     * @param statisticsQueryParam 统计查询参数
     * @return 商品概况
     */
    GoodsOverviewVO overview(StatisticsQueryParam statisticsQueryParam);

    /**
     * 退款商品排行
     *
     * @param statisticsQueryParam 统计查询参数
     * @return 排行列表
     */
    List<GoodsRankVO> refundRank(StatisticsQueryParam statisticsQueryParam);

    /**
     * 销量商品排行
     *
     * @param statisticsQueryParam 统计查询参数
     * @return 排行列表
     */
    List<GoodsRankVO> salesRank(StatisticsQueryParam statisticsQueryParam);
}
