package cn.lili.modules.order.cart.render;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.render.impl.CartPriceRender;
import cn.lili.modules.order.cart.render.impl.SkuPromotionRender;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * E_COUPON 专用渲染链须回填 basePrice，避免 flowPrice=0。
 *
 * @author Mike
 * @date 2026-07-31
 */
class ECouponPriceRenderTest {

    private SkuPromotionRender skuPromotionRender;
    private CartPriceRender cartPriceRender;
    private CardKeyService cardKeyService;

    @BeforeEach
    void setUp() {
        skuPromotionRender = new SkuPromotionRender();
        cartPriceRender = new CartPriceRender();
        cardKeyService = mock(CardKeyService.class);
        ReflectionTestUtils.setField(skuPromotionRender, "cardKeyService", cardKeyService);
    }

    @Test
    void eCouponTradeRenderStepsProducePositiveFlowPrice() {
        TradeDTO tradeDTO = buildECouponTrade(2, 158.0);
        when(cardKeyService.isECouponOnlyTrade(tradeDTO)).thenReturn(true);

        skuPromotionRender.render(tradeDTO);
        cartPriceRender.render(tradeDTO);

        assertThat(tradeDTO.getPriceDetailDTO().getFlowPrice()).isEqualTo(316.0);
        assertThat(tradeDTO.getCheckedSkuList().get(0).getPriceDetailDTO().getGoodsPrice()).isEqualTo(316.0);
    }

    private TradeDTO buildECouponTrade(int num, double unitPrice) {
        GoodsSku goodsSku = new GoodsSku();
        goodsSku.setId("sku-ecoupon");
        goodsSku.setGoodsType(GoodsTypeEnum.E_COUPON.name());
        goodsSku.setPrice(unitPrice);
        goodsSku.setStoreId("store-1");

        CartSkuVO cartSkuVO = new CartSkuVO(goodsSku);
        cartSkuVO.setNum(num);
        cartSkuVO.setSubTotal(unitPrice * num);
        cartSkuVO.setCartType(CartTypeEnum.BUY_NOW);
        cartSkuVO.setChecked(true);

        CartVO cartVO = new CartVO(cartSkuVO);
        cartVO.setSkuList(List.of(cartSkuVO));

        TradeDTO tradeDTO = new TradeDTO(CartTypeEnum.BUY_NOW);
        tradeDTO.setSkuList(List.of(cartSkuVO));
        tradeDTO.setCartList(List.of(cartVO));
        return tradeDTO;
    }
}
