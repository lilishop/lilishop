package cn.lili.cache.config.redis;

import cn.lili.modules.order.cart.entity.dto.MemberCouponDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FastJsonRedisSerializerTest {

    @Test
    void deserializeTradeDtoWithEmptyTypedStoreCoupons() {
        String json = "{\"@type\":\"cn.lili.modules.order.cart.entity.dto.TradeDTO\","
                + "\"cartTypeEnum\":\"CART\","
                + "\"skuPromotionDetail\":{\"@type\":\"java.util.HashMap\"},"
                + "\"storeCoupons\":{\"@type\":\"java.util.HashMap\"}}";

        FastJsonRedisSerializer<Object> serializer = new FastJsonRedisSerializer<>(Object.class);
        Object result = assertDoesNotThrow(() -> serializer.deserialize(json.getBytes(StandardCharsets.UTF_8)));
        assertNotNull(result);
    }

    @Test
    void normalizeEmptyTypedMapsOnlyReplacesEmptyHashMap() {
        String input = "{\"empty\":{\"@type\":\"java.util.HashMap\"},"
                + "\"filled\":{\"@type\":\"java.util.HashMap\",\"k\":{\"@type\":\"java.lang.String\",\"value\":\"v\"}}}";
        String normalized = FastJsonRedisSerializer.prepareRedisJson(input);
        assertEquals("{\"empty\":{},"
                + "\"filled\":{\"@type\":\"java.util.HashMap\",\"k\":{\"@type\":\"java.lang.String\",\"value\":\"v\"}}}", normalized);
    }

    @Test
    void roundTripBuyNowTradeDto() {
        FastJsonRedisSerializer<Object> serializer = new FastJsonRedisSerializer<>(Object.class);
        TradeDTO dto = new TradeDTO(CartTypeEnum.BUY_NOW);
        byte[] bytes = serializer.serialize(dto);
        assertDoesNotThrow(() -> serializer.deserialize(bytes));
    }

    @Test
    void roundTripTradeDtoWithPopulatedStoreCoupon() {
        FastJsonRedisSerializer<Object> serializer = new FastJsonRedisSerializer<>(Object.class);
        TradeDTO dto = new TradeDTO(CartTypeEnum.BUY_NOW);
        Map<String, MemberCouponDTO> storeCoupons = new HashMap<>();
        storeCoupons.put(
                "store-1",
                new MemberCouponDTO(Map.of("sku-1", 158D), new MemberCoupon())
        );
        dto.setStoreCoupons(storeCoupons);

        byte[] bytes = serializer.serialize(dto);
        Object result = assertDoesNotThrow(() -> serializer.deserialize(bytes));

        assertTrue(result instanceof TradeDTO);
        TradeDTO restored = (TradeDTO) result;
        assertEquals(158D, restored.getStoreCoupons().get("store-1").getSkuDetail().get("sku-1"));
    }

    @Test
    void redisPayloadDoesNotContainDerivedCheckedSkuList() {
        FastJsonRedisSerializer<Object> serializer = new FastJsonRedisSerializer<>(Object.class);
        TradeDTO dto = new TradeDTO(CartTypeEnum.CART);

        String json = new String(serializer.serialize(dto), StandardCharsets.UTF_8);

        assertFalse(json.contains("\"checkedSkuList\""));
    }
}
