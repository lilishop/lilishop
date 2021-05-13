package cn.lili.modules.order.cart.render.impl;

import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

/**
 * 商品有效性校验
 *
 * @author Chopper
 * @date 2020-07-02 14:47
 */
@Order(0)
@Service
public class CheckDataRender implements CartRenderStep {

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    public void render(TradeDTO tradeDTO) {
        for (CartSkuVO cartSkuVO : tradeDTO.getSkuList()) {
            GoodsSku dataSku = goodsSkuService.getGoodsSkuByIdFromCache(cartSkuVO.getGoodsSku().getId());
            if (dataSku == null || dataSku.getUpdateTime().before(cartSkuVO.getGoodsSku().getUpdateTime())) {
                //设置购物车未选中
                cartSkuVO.setChecked(false);
                //设置购物车此sku商品已失效
                cartSkuVO.setInvalid(true);
                //设置失效消息
                cartSkuVO.setErrorMessage("商品信息发生变化,已失效");
                continue;
            }
            if (!GoodsAuthEnum.PASS.name().equals(dataSku.getIsAuth()) || !GoodsStatusEnum.UPPER.name().equals(dataSku.getMarketEnable())) {
                //设置购物车未选中
                cartSkuVO.setChecked(false);
                //设置购物车此sku商品已失效
                cartSkuVO.setInvalid(true);
                //设置失效消息
                cartSkuVO.setErrorMessage("商品已下架");
                continue;
            }
            //商品库存不足
            if (dataSku.getQuantity() <= 0) {
                //设置购物车未选中
                cartSkuVO.setChecked(false);
                //设置购物车此sku商品已失效
                cartSkuVO.setInvalid(true);
                //设置失效消息
                cartSkuVO.setErrorMessage("商品库存不足");
            }
            //写入初始价格
            cartSkuVO.getPriceDetailDTO().setGoodsPrice(CurrencyUtil.mul(cartSkuVO.getPurchasePrice(), cartSkuVO.getNum()));
        }
    }
}
