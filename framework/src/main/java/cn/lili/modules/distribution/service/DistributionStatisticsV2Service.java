package cn.lili.modules.distribution.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dto.DistributionStatisticsQueryParams;
import cn.lili.modules.distribution.entity.vos.DistributionBehaviorDataVO;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsDataVO;
import cn.lili.modules.distribution.entity.vos.DistributionOverviewV2VO;
import com.baomidou.mybatisplus.core.metadata.IPage;

/**
 * 分销统计 V2 服务。
 * <p>
 * 提供分销概览、行为数据及商品数据等多维度统计查询。
 * </p>
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionStatisticsV2Service {

    /**
     * 查询分销概览统计数据。
     *
     * @param params 统计查询条件（时间范围等）
     * @return 概览数据
     */
    DistributionOverviewV2VO overview(DistributionStatisticsQueryParams params);

    /**
     * 分页查询分销行为数据（分享、访问等）。
     *
     * @param params 统计查询条件
     * @param page   分页参数
     * @return 行为数据分页
     */
    IPage<DistributionBehaviorDataVO> behaviorDataPage(DistributionStatisticsQueryParams params, PageVO page);

    /**
     * 分页查询分销商品数据（推广、成交等）。
     *
     * @param params 统计查询条件
     * @param page   分页参数
     * @return 商品数据分页
     */
    IPage<DistributionGoodsDataVO> goodsDataPage(DistributionStatisticsQueryParams params, PageVO page);
}
