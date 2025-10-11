package cn.lili.modules.statistics.service;

import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.BusinessCompositionDataVO;
import cn.lili.modules.statistics.entity.vo.OrderOverviewVO;
import cn.lili.modules.statistics.entity.vo.OverViewDataVO;
import cn.lili.modules.statistics.entity.vo.SourceDataVO;

import java.util.Date;
import java.util.List;

/**
 * 营业概览统计
 *
 * @author Bulbasaur
 * @since 2025/08/25 7:31 下午
 */
public interface OverViewStatisticsService {


    /**
     * 获取营业概览统计
     *
     * @param statisticsQueryParam 统计参数
     * @return 营业概览统计
     */
    OverViewDataVO getOverViewDataVO(StatisticsQueryParam statisticsQueryParam);


    /**
     * 获取收款构成列表
     *
     * @param statisticsQueryParam 统计参数
     * @return 收款构成列表
     */
    List<SourceDataVO> getSourceDataVOList(StatisticsQueryParam statisticsQueryParam);

    /**
     * 获取营业构成列表
     *
     * @param statisticsQueryParam 统计参数
     * @return 营业构成列表
     */
    BusinessCompositionDataVO businessCompositionDataVO(StatisticsQueryParam statisticsQueryParam);


}
