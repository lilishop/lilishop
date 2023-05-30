package cn.lili.modules.order.cart.render.impl;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import com.xkcoding.http.util.StringUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

/**
 * 分销佣金计算
 *
 * @author Chopper
 * @since 2020-07-02 14:47
 */
@Service
public class DistributionPriceRender implements CartRenderStep {
    /**
     * 缓存
     */
    @Autowired
    private Cache cache;

    @Autowired
    private DistributionGoodsService distributionGoodsService;

    @Override
    public RenderStepEnums step() {
        return RenderStepEnums.DISTRIBUTION;
    }

    @Override
    public void render(TradeDTO tradeDTO) {
        this.renderDistribution(tradeDTO);
    }

    /**
     * 渲染分销佣金
     *
     * @param tradeDTO
     */
    private void renderDistribution(TradeDTO tradeDTO) {

        //如果存在分销员
        String distributionId = (String) cache.get(CachePrefix.DISTRIBUTION.getPrefix() + "_" + tradeDTO.getMemberId());
        if (StringUtil.isEmpty(distributionId)) {
            return;
        }
        //循环订单商品列表，如果是分销商品则计算商品佣金
        tradeDTO.setDistributionId(distributionId);

        List<String> skuIds = tradeDTO.getCheckedSkuList().stream().map(cartSkuVO -> {
            return cartSkuVO.getGoodsSku().getId();
        }).collect(Collectors.toList());
        //是否包含分销商品
        List<DistributionGoods> distributionGoods = distributionGoodsService.distributionGoods(skuIds);
        if (distributionGoods != null && !distributionGoods.isEmpty()) {
            distributionGoods.forEach(dg -> tradeDTO.getCheckedSkuList().forEach(cartSkuVO -> {
                if (cartSkuVO.getGoodsSku().getId().equals(dg.getSkuId())) {
                    cartSkuVO.setDistributionGoods(dg);
                }
            }));
        }

        for (CartSkuVO cartSkuVO : tradeDTO.getCheckedSkuList()) {
            if (cartSkuVO.getDistributionGoods() != null) {
                cartSkuVO.getPriceDetailDTO().setDistributionCommission(CurrencyUtil.mul(cartSkuVO.getNum(), cartSkuVO.getDistributionGoods().getCommission()));
            }
        }

    }
}
