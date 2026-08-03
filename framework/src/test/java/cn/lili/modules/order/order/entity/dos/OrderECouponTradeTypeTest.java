package cn.lili.modules.order.order.entity.dos;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * E_COUPON 下单落库：orderType 与虚拟发货归一化。
 *
 * @author Mike
 * @date 2026-08-02
 */
class OrderECouponTradeTypeTest {

    @Test
    void buyNowECouponSetsOrderTypeAndVirtualDelivery() {
        TradeDTO tradeDTO = new TradeDTO(CartTypeEnum.BUY_NOW);
        tradeDTO.setSn("T001");
        CartVO cartVO = cartWithECoupon(tradeDTO, 1);

        Order order = new Order(cartVO, tradeDTO);

        assertThat(order.getOrderType()).isEqualTo(OrderTypeEnum.E_COUPON.name());
        assertThat(order.getOrderPromotionType()).isEqualTo(OrderPromotionTypeEnum.NORMAL.name());
        assertThat(order.getDeliveryMethod()).isEqualTo(DeliveryMethodEnum.VIRTUAL.name());
        assertThat(order.getFreightPrice()).isZero();
        assertThat(order.getConsigneeName()).isNull();
        assertThat(order.getPayStatus()).isEqualTo(PayStatusEnum.UNPAID.name());
    }

    @Test
    void pintuanECouponSetsOrderTypeECouponNotVirtual() {
        TradeDTO tradeDTO = new TradeDTO(CartTypeEnum.PINTUAN);
        tradeDTO.setSn("T-PT");
        CartSkuVO cartSkuVO = eCouponLine(1);
        Map<String, Object> promotionMap = new HashMap<>();
        promotionMap.put("PINTUAN-activity-1", "promo");
        cartSkuVO.setPromotionMap(promotionMap);
        CartVO cartVO = new CartVO(cartSkuVO);
        cartVO.setSkuList(List.of(cartSkuVO));
        cartVO.getPriceDetailDTO().setFreightPrice(5D);
        tradeDTO.setSkuList(List.of(cartSkuVO));
        tradeDTO.setCartList(List.of(cartVO));

        Order order = new Order(cartVO, tradeDTO);

        assertThat(order.getOrderType()).isEqualTo(OrderTypeEnum.E_COUPON.name());
        assertThat(order.getOrderPromotionType()).isEqualTo(OrderPromotionTypeEnum.PINTUAN.name());
        assertThat(order.getDeliveryMethod()).isEqualTo(DeliveryMethodEnum.VIRTUAL.name());
        assertThat(order.getFreightPrice()).isZero();
    }

    private CartVO cartWithECoupon(TradeDTO tradeDTO, int num) {
        CartSkuVO cartSkuVO = eCouponLine(num);
        CartVO cartVO = new CartVO(cartSkuVO);
        cartVO.setSkuList(List.of(cartSkuVO));
        cartVO.setDeliveryMethod(DeliveryMethodEnum.LOGISTICS.name());
        cartVO.getPriceDetailDTO().setFreightPrice(8D);
        tradeDTO.setSkuList(List.of(cartSkuVO));
        tradeDTO.setCartList(List.of(cartVO));
        return cartVO;
    }

    private CartSkuVO eCouponLine(int num) {
        GoodsSku goodsSku = new GoodsSku();
        goodsSku.setId("sku-ecoupon");
        goodsSku.setGoodsType(GoodsTypeEnum.E_COUPON.name());
        goodsSku.setPrice(99D);
        goodsSku.setGoodsName("测试卡密");
        goodsSku.setCategoryPath("1,2,3");
        CartSkuVO cartSkuVO = new CartSkuVO(goodsSku);
        cartSkuVO.setNum(num);
        cartSkuVO.setChecked(true);
        cartSkuVO.setPurchasePrice(99D);
        cartSkuVO.setSubTotal(99D * num);
        return cartSkuVO;
    }
}
