package cn.lili.modules.order.cart.render.impl;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
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

    @Autowired
    private DistributionCustomerRelationService customerRelationService;

    /**
     * 返回分销佣金渲染步骤标识。
     */
    @Override
    public RenderStepEnums step() {
        return RenderStepEnums.DISTRIBUTION;
    }

    /**
     * 结算前解析分销员并挂载分销商品佣金信息。
     *
     * @param tradeDTO 交易上下文
     */
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

        String distributionId = null;
        // 优先取客户有效绑定关系中的分销员
        DistributionCustomerRelation relation = customerRelationService.getActiveRelation(tradeDTO.getMemberId());
        if (relation != null && relation.getDistributionId() != null) {
            distributionId = relation.getDistributionId();
        }
        // 无绑定关系时回退到分享/扫码写入的缓存分销员
        if (StringUtil.isEmpty(distributionId)) {
            distributionId = (String) cache.get(CachePrefix.DISTRIBUTION.getPrefix() + "_" + tradeDTO.getMemberId());
        }
        if (StringUtil.isEmpty(distributionId)) {
            return;
        }
        tradeDTO.setDistributionId(distributionId);

        List<String> skuIds = tradeDTO.getCheckedSkuList().stream().map(cartSkuVO -> {
            return cartSkuVO.getGoodsSku().getId();
        }).collect(Collectors.toList());
        // 批量查询勾选 SKU 对应的分销商品配置
        List<DistributionGoods> distributionGoods = distributionGoodsService.distributionGoods(skuIds);
        if (distributionGoods != null && !distributionGoods.isEmpty()) {
            // 将分销商品信息挂到对应购物车行，供后续结算与分佣使用
            distributionGoods.forEach(dg -> tradeDTO.getCheckedSkuList().forEach(cartSkuVO -> {
                if (cartSkuVO.getGoodsSku().getId().equals(dg.getSkuId())) {
                    cartSkuVO.setDistributionGoods(dg);
                }
            }));
        }

        for (CartSkuVO cartSkuVO : tradeDTO.getCheckedSkuList()) {
            if (cartSkuVO.getDistributionGoods() != null) {
                // 行佣金 = 单品固定佣金 × 购买数量
                cartSkuVO.getPriceDetailDTO().setDistributionCommission(CurrencyUtil.mul(cartSkuVO.getNum(), cartSkuVO.getDistributionGoods().getCommission()));
            }
        }

    }
}
