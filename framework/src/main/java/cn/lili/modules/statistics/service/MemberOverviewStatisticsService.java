package cn.lili.modules.statistics.service;

import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.MemberAnalysisVO;
import cn.lili.modules.statistics.entity.vo.MemberGenderDistributionVO;
import cn.lili.modules.statistics.entity.vo.MemberNewTrendVO;
import cn.lili.modules.statistics.entity.vo.MemberOverviewVO;
import cn.lili.modules.statistics.entity.vo.MemberRegionDistributionVO;

import java.util.List;

/**
 * 会员概况与客户分析统计业务层
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
public interface MemberOverviewStatisticsService {

    /**
     * 会员概况指标
     *
     * @param param 统计查询参数
     * @return 会员概况
     */
    MemberOverviewVO overview(StatisticsQueryParam param);

    /**
     * 新增会员趋势
     *
     * @param param 统计查询参数
     * @return 按日新增趋势
     */
    List<MemberNewTrendVO> newMemberTrend(StatisticsQueryParam param);

    /**
     * 客户分析指标
     *
     * @param param 统计查询参数
     * @return 客户分析
     */
    MemberAnalysisVO analysis(StatisticsQueryParam param);

    /**
     * 会员性别分布
     *
     * @return 性别分布列表
     */
    List<MemberGenderDistributionVO> genderDistribution();

    /**
     * 会员地域分布
     *
     * @return 地域分布列表
     */
    List<MemberRegionDistributionVO> regionDistribution();
}
