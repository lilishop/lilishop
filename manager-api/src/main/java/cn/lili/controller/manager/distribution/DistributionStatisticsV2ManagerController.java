package cn.lili.controller.manager.distribution;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dto.DistributionStatisticsQueryParams;
import cn.lili.modules.distribution.entity.vos.DistributionBehaviorDataVO;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsDataVO;
import cn.lili.modules.distribution.entity.vos.DistributionOverviewV2VO;
import cn.lili.modules.distribution.service.DistributionStatisticsV2Service;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,分销统计V2接口
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@RestController
@Tag(name = "管理端,分销统计V2接口")
@RequestMapping("/manager/statistics/distribution/v2")
public class DistributionStatisticsV2ManagerController {

    @Autowired
    private DistributionStatisticsV2Service statisticsV2Service;

    @Operation(summary = "分销概览V2")
    @GetMapping("/overview")
    public ResultMessage<DistributionOverviewV2VO> overview(DistributionStatisticsQueryParams params) {
        return ResultUtil.data(statisticsV2Service.overview(params));
    }

    @Operation(summary = "行为数据分页")
    @GetMapping("/behavior")
    public ResultMessage<IPage<DistributionBehaviorDataVO>> behavior(DistributionStatisticsQueryParams params, PageVO page) {
        return ResultUtil.data(statisticsV2Service.behaviorDataPage(params, page));
    }

    @Operation(summary = "商品数据分页")
    @GetMapping("/goods")
    public ResultMessage<IPage<DistributionGoodsDataVO>> goods(DistributionStatisticsQueryParams params, PageVO page) {
        return ResultUtil.data(statisticsV2Service.goodsDataPage(params, page));
    }
}
