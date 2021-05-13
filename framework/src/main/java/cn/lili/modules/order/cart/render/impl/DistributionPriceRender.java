package cn.lili.modules.order.cart.render.impl;

import cn.lili.common.cache.Cache;
import cn.lili.common.cache.CachePrefix;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

/**
 * 购物促销信息渲染实现
 *
 * @author Chopper
 * @date 2020-07-02 14:47
 */
@Service
@Order(6)
public class DistributionPriceRender implements CartRenderStep {
    /**
     * 缓存
     */
    @Autowired
    private Cache cache;

    @Override
    public void render(TradeDTO tradeDTO) {
        //主要渲染各个优惠的价格
        this.renderDistribution(tradeDTO);
    }

    /**
     * 渲染分销佣金
     *
     * @param tradeDTO
     */
    private void renderDistribution(TradeDTO tradeDTO) {

        if(cache.get(CachePrefix.DISTRIBUTION.getPrefix()+"_"+tradeDTO.getMemberId())==null){
            return;
        }
        //循环订单商品列表，如果是分销商品则计算商品佣金
        tradeDTO.setDistributionId(cache.get(CachePrefix.DISTRIBUTION.getPrefix()+"_"+tradeDTO.getMemberId()).toString());
        for (CartSkuVO cartSkuVO: tradeDTO.getSkuList()) {
            if(cartSkuVO.getDistributionGoods()!=null){
                cartSkuVO.getPriceDetailDTO().setDistributionCommission(CurrencyUtil.mul(cartSkuVO.getNum(), cartSkuVO.getDistributionGoods().getCommission()));
            }

        }

    }
}
