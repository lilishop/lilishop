package cn.lili.controller.distribution;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dto.DistributionGoodsSearchParams;
import cn.lili.modules.distribution.entity.vos.DistributionFeaturedGoodsItemVO;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsPosterVO;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsShareVO;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsVO;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.distribution.service.DistributionFeaturedGoodsService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.support.DistributionBuyerCommissionSupport;
import cn.lili.modules.distribution.support.DistributionGoodsSettingSupport;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.system.entity.dto.DistributionFeaturedGoodsSetting;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * 买家端,分销商品接口
 *
 * @author Bulbasaur
 * @since 2020/11/16 10:06 下午
 */
@RestController
@Tag(name = "买家端,分销商品接口")
@RequestMapping("/buyer/distribution/goods")
public class DistributionGoodsBuyerController {

    @Autowired
    private DistributionGoodsService distributionGoodsService;

    @Autowired
    private DistributionFeaturedGoodsService distributionFeaturedGoodsService;

    @Autowired
    private DistributionService distributionService;

    @Autowired
    private DistributionBuyerCommissionSupport buyerCommissionSupport;

    @Autowired
    private DistributionGoodsSettingSupport distributionGoodsSettingSupport;

    @Operation(summary = "获取分销商品设置")
    @GetMapping("/setting")
    public ResultMessage<DistributionFeaturedGoodsSetting> goodsSetting() {
        return ResultUtil.data(distributionGoodsSettingSupport.readSetting());
    }

    @Operation(summary = "获取分销员商品列表", description = "获取分销员商品列表")
    @GetMapping
    public ResultMessage<IPage<DistributionGoodsVO>> distributionGoods(DistributionGoodsSearchParams distributionGoodsSearchParams) {
        return ResultUtil.data(distributionGoodsService.goodsPage(distributionGoodsSearchParams));
    }

    @Operation(summary = "获取主推商品列表")
    @GetMapping("/featured")
    public ResultMessage<java.util.List<DistributionFeaturedGoodsItemVO>> featuredGoods() {
        distributionService.checkDistributionSetting();
        Distribution distribution = distributionService.getDistribution();
        java.util.List<DistributionFeaturedGoodsItemVO> list = distributionFeaturedGoodsService.listFeaturedForBuyer();
        if (distribution != null && !distributionService.isRetreated(distribution.getMemberId())) {
            buyerCommissionSupport.applyToFeaturedGoods(list, distribution);
        }
        return ResultUtil.data(list);
    }

    @Operation(summary = "获取分销商品分享上下文")
    @GetMapping("/share-context")
    public ResultMessage<DistributionGoodsShareVO> shareContext(
            @RequestParam String skuId,
            @RequestParam String goodsId,
            @RequestParam String distributionId) {
        return ResultUtil.data(distributionGoodsService.getShareContext(skuId, goodsId, distributionId));
    }

    @Operation(summary = "获取分销商品海报上下文（生成葵花码并记录分享）")
    @GetMapping("/poster-context")
    public ResultMessage<DistributionGoodsPosterVO> posterContext(
            @RequestParam String skuId,
            @RequestParam String goodsId,
            @RequestParam String distributionId) {
        return ResultUtil.data(distributionGoodsService.getPosterContext(skuId, goodsId, distributionId));
    }
}
