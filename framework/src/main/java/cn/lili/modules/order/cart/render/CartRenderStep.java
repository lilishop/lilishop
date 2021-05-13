package cn.lili.modules.order.cart.render;

import cn.lili.modules.order.cart.entity.dto.TradeDTO;

/**
 * 购物车渲染
 *
 * @author Chopper
 * @date 2020-04-01 10:27 上午
 */
public interface CartRenderStep {


    /**
     * 渲染一笔交易
     * 0-> 校验商品 1-》 满优惠渲染 2->渲染优惠 3->优惠券渲染 4->计算运费 5->计算价格 6->分销渲染 7->其他渲染
     *
     * @param tradeDTO
     */
    void render(TradeDTO tradeDTO);
}
