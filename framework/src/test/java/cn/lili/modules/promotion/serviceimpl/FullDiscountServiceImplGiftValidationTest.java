package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.order.cart.render.impl.FullDiscountRender;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dto.FullDiscountGiftItem;
import cn.lili.modules.promotion.entity.dto.FullDiscountRuleDTO;
import cn.lili.modules.promotion.entity.enums.FullDiscountTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * 满减、满折、满赠活动三级阶梯校验、同店SKU互斥校验与购物车渲染测试。
 */
class FullDiscountServiceImplGiftValidationTest {

    private FullDiscountServiceImpl fullDiscountService;
    private GoodsSkuService goodsSkuService;
    private PromotionGoodsService promotionGoodsService;

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        fullDiscountService = spy(new FullDiscountServiceImpl());
        goodsSkuService = mock(GoodsSkuService.class);
        promotionGoodsService = mock(PromotionGoodsService.class);
        ReflectionTestUtils.setField(fullDiscountService, "goodsSkuService", goodsSkuService);
        ReflectionTestUtils.setField(fullDiscountService, "promotionGoodsService", promotionGoodsService);
        doReturn(true).when(fullDiscountService).allowExistSame();
        doReturn(Collections.emptyList()).when(fullDiscountService).list(any(Wrapper.class));
    }

    /**
     * 验证满额赠活动允许使用卡密赠品SKU。
     */
    @Test
    void checkPromotionsAllowsECouponGiftSku() {
        GoodsSku giftSku = buildGiftSku("SKU001", GoodsTypeEnum.E_COUPON);
        when(goodsSkuService.getGoodsSkuByIdFromCache("SKU001")).thenReturn(giftSku);

        assertDoesNotThrow(() -> fullDiscountService.checkPromotions(buildGiftPromotion("SKU001")));
    }

    /**
     * 验证满额赠活动允许使用实物赠品SKU。
     */
    @Test
    void checkPromotionsAllowsPhysicalGiftSku() {
        GoodsSku giftSku = buildGiftSku("SKU002", GoodsTypeEnum.PHYSICAL_GOODS);
        when(goodsSkuService.getGoodsSkuByIdFromCache("SKU002")).thenReturn(giftSku);

        assertDoesNotThrow(() -> fullDiscountService.checkPromotions(buildGiftPromotion("SKU002")));
    }

    /**
     * 验证满额赠活动拒绝不存在的赠品SKU。
     */
    @Test
    void checkPromotionsRejectsMissingGiftSku() {
        when(goodsSkuService.getGoodsSkuByIdFromCache("MISSING")).thenReturn(null);

        ServiceException ex = assertThrows(ServiceException.class,
                () -> fullDiscountService.checkPromotions(buildGiftPromotion("MISSING")));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.FULL_DISCOUNT_GIFT_ERROR);
    }

    /**
     * 验证满减活动支持1~3级阶梯递增校验，且自动回填一档兼容字段。
     */
    @Test
    void checkPromotionsValidatesFullMinusTiersAndSyncsFirstTierFields() {
        FullDiscountVO vo = buildBaseVO(FullDiscountTypeEnum.FULL_MINUS.name());
        FullDiscountRuleDTO tier1 = new FullDiscountRuleDTO();
        tier1.setFullMoney(100D);
        tier1.setFullMinus(10D);
        FullDiscountRuleDTO tier2 = new FullDiscountRuleDTO();
        tier2.setFullMoney(200D);
        tier2.setFullMinus(25D);
        FullDiscountRuleDTO tier3 = new FullDiscountRuleDTO();
        tier3.setFullMoney(300D);
        tier3.setFullMinus(50D);
        vo.setRuleList(Arrays.asList(tier1, tier2, tier3));

        assertDoesNotThrow(() -> fullDiscountService.checkPromotions(vo));
        assertThat(vo.getFullMoney()).isEqualTo(100D);
        assertThat(vo.getFullMinus()).isEqualTo(10D);
        assertThat(vo.getFullMinusFlag()).isTrue();
        assertThat(vo.getRulesJson()).isNotBlank();
    }

    /**
     * 验证满减活动阶梯门槛未严格递增或立减金额未递增时抛出异常。
     */
    @Test
    void checkPromotionsRejectsNonIncreasingFullMinusTiers() {
        FullDiscountVO vo = buildBaseVO(FullDiscountTypeEnum.FULL_MINUS.name());
        FullDiscountRuleDTO tier1 = new FullDiscountRuleDTO();
        tier1.setFullMoney(200D);
        tier1.setFullMinus(20D);
        FullDiscountRuleDTO tier2 = new FullDiscountRuleDTO();
        tier2.setFullMoney(200D);
        tier2.setFullMinus(30D);
        vo.setRuleList(Arrays.asList(tier1, tier2));

        ServiceException thresholdEx = assertThrows(ServiceException.class,
                () -> fullDiscountService.checkPromotions(vo));
        assertThat(thresholdEx.getResultCode()).isEqualTo(ResultCode.FULL_DISCOUNT_TIER_THRESHOLD_ERROR);

        tier2.setFullMoney(300D);
        tier2.setFullMinus(15D);
        ServiceException minusEx = assertThrows(ServiceException.class,
                () -> fullDiscountService.checkPromotions(vo));
        assertThat(minusEx.getResultCode()).isEqualTo(ResultCode.FULL_DISCOUNT_TIER_MINUS_ERROR);
    }

    /**
     * 验证满折活动阶梯折扣率必须严格递减（力度递增）。
     */
    @Test
    void checkPromotionsValidatesFullRateDecreasingDiscount() {
        FullDiscountVO vo = buildBaseVO(FullDiscountTypeEnum.FULL_RATE.name());
        FullDiscountRuleDTO tier1 = new FullDiscountRuleDTO();
        tier1.setFullMoney(100D);
        tier1.setFullRate(9.0D);
        FullDiscountRuleDTO tier2 = new FullDiscountRuleDTO();
        tier2.setFullMoney(200D);
        tier2.setFullRate(9.5D);
        vo.setRuleList(Arrays.asList(tier1, tier2));

        ServiceException rateEx = assertThrows(ServiceException.class,
                () -> fullDiscountService.checkPromotions(vo));
        assertThat(rateEx.getResultCode()).isEqualTo(ResultCode.FULL_DISCOUNT_TIER_RATE_ERROR);

        tier2.setFullRate(8.0D);
        assertDoesNotThrow(() -> fullDiscountService.checkPromotions(vo));
        assertThat(vo.getFullRateFlag()).isTrue();
        assertThat(vo.getFullRate()).isEqualTo(9.0D);
    }

    /**
     * 验证满赠活动每档赠品SKU数量限制在1~3种、每种件数限制在1~10件。
     */
    @Test
    void checkPromotionsValidatesFullGiftSkuCountAndQuantityBounds() {
        for (int i = 1; i <= 4; i++) {
            when(goodsSkuService.getGoodsSkuByIdFromCache("G" + i))
                    .thenReturn(buildGiftSku("G" + i, GoodsTypeEnum.PHYSICAL_GOODS));
        }

        FullDiscountVO vo = buildBaseVO(FullDiscountTypeEnum.FULL_GIFT.name());
        FullDiscountRuleDTO tier1 = new FullDiscountRuleDTO();
        tier1.setFullMoney(100D);
        tier1.setGiftItems(Arrays.asList(
                buildGiftItem("G1", 1),
                buildGiftItem("G2", 1),
                buildGiftItem("G3", 1),
                buildGiftItem("G4", 1)
        ));
        vo.setRuleList(Collections.singletonList(tier1));

        ServiceException sizeEx = assertThrows(ServiceException.class,
                () -> fullDiscountService.checkPromotions(vo));
        assertThat(sizeEx.getResultCode()).isEqualTo(ResultCode.FULL_DISCOUNT_GIFT_SIZE_ERROR);

        tier1.setGiftItems(Collections.singletonList(buildGiftItem("G1", 11)));
        ServiceException numEx = assertThrows(ServiceException.class,
                () -> fullDiscountService.checkPromotions(vo));
        assertThat(numEx.getResultCode()).isEqualTo(ResultCode.FULL_DISCOUNT_GIFT_NUM_ERROR);

        tier1.setGiftItems(Arrays.asList(buildGiftItem("G1", 2), buildGiftItem("G2", 5)));
        assertDoesNotThrow(() -> fullDiscountService.checkPromotions(vo));
        assertThat(vo.getGiftFlag()).isTrue();
        assertThat(vo.getGiftId()).isEqualTo("G1,G2");
    }

    /**
     * 验证同店铺同一时段部分商品活动在SKU不重叠时允许共存，SKU重叠或存在全店活动时冲突报错。
     */
    @Test
    void checkPromotionsAllowsDisjointSkusAndRejectsOverlappingSkus() {
        FullDiscount existingPortion = new FullDiscount();
        existingPortion.setId("ACT_1");
        existingPortion.setStoreId("STORE_1");
        existingPortion.setScopeType(PromotionsScopeTypeEnum.PORTION_GOODS.name());
        existingPortion.setScopeId("SKU_A,SKU_B");
        existingPortion.setStartTime(new Date(System.currentTimeMillis() - 86_400_000L));
        existingPortion.setEndTime(new Date(System.currentTimeMillis() + 86_400_000L * 30));
        doReturn(Collections.singletonList(existingPortion)).when(fullDiscountService).list(any(Wrapper.class));

        FullDiscountVO newPortionDisjoint = buildBaseVO(FullDiscountTypeEnum.FULL_MINUS.name());
        newPortionDisjoint.setStoreId("STORE_1");
        newPortionDisjoint.setScopeType(PromotionsScopeTypeEnum.PORTION_GOODS.name());
        newPortionDisjoint.setScopeId("SKU_C,SKU_D");
        FullDiscountRuleDTO rule = new FullDiscountRuleDTO();
        rule.setFullMoney(100D);
        rule.setFullMinus(10D);
        newPortionDisjoint.setRuleList(Collections.singletonList(rule));

        assertDoesNotThrow(() -> fullDiscountService.checkPromotions(newPortionDisjoint));

        newPortionDisjoint.setScopeId("SKU_B,SKU_D");
        ServiceException overlapEx = assertThrows(ServiceException.class,
                () -> fullDiscountService.checkPromotions(newPortionDisjoint));
        assertThat(overlapEx.getResultCode()).isEqualTo(ResultCode.PROMOTION_SKU_CONFLICT);
    }

    /**
     * 验证购物车内同店多个满额活动分组计算并按最高满足阶梯生效。
     */
    @Test
    void fullDiscountRenderMatchesHighestTierAndSupportsMultipleActivities() {
        GoodsSkuService renderGoodsSkuService = mock(GoodsSkuService.class);
        GoodsSku giftSku1 = buildGiftSku("GIFT_1", GoodsTypeEnum.PHYSICAL_GOODS);
        GoodsSku giftSku2 = buildGiftSku("GIFT_2", GoodsTypeEnum.PHYSICAL_GOODS);
        when(renderGoodsSkuService.getGoodsSkuByIdFromCache("GIFT_1")).thenReturn(giftSku1);
        when(renderGoodsSkuService.getGoodsSkuByIdFromCache("GIFT_2")).thenReturn(giftSku2);

        FullDiscountRender render = new FullDiscountRender();
        ReflectionTestUtils.setField(render, "goodsSkuService", renderGoodsSkuService);

        // 活动1：满减活动（满100减10，满200减30），绑定 SKU_1（小计250元，命中二档减30元）
        FullDiscount minusAct = new FullDiscount();
        minusAct.setId("ACT_MINUS");
        minusAct.setStoreId("STORE_1");
        minusAct.setActivityType(FullDiscountTypeEnum.FULL_MINUS.name());
        minusAct.setStartTime(new Date(System.currentTimeMillis() - 60_000L));
        minusAct.setEndTime(new Date(System.currentTimeMillis() + 60_000L));
        FullDiscountRuleDTO m1 = new FullDiscountRuleDTO();
        m1.setFullMoney(100D);
        m1.setFullMinus(10D);
        FullDiscountRuleDTO m2 = new FullDiscountRuleDTO();
        m2.setFullMoney(200D);
        m2.setFullMinus(30D);
        minusAct.setRulesJson(JSONUtil.toJsonStr(Arrays.asList(m1, m2)));

        // 活动2：满赠活动（满100送GIFT_1x2、GIFT_2x3），绑定 SKU_2（小计120元，命中一档）
        FullDiscount giftAct = new FullDiscount();
        giftAct.setId("ACT_GIFT");
        giftAct.setStoreId("STORE_1");
        giftAct.setActivityType(FullDiscountTypeEnum.FULL_GIFT.name());
        giftAct.setStartTime(new Date(System.currentTimeMillis() - 60_000L));
        giftAct.setEndTime(new Date(System.currentTimeMillis() + 60_000L));
        FullDiscountRuleDTO g1 = new FullDiscountRuleDTO();
        g1.setFullMoney(100D);
        g1.setGiftItems(Arrays.asList(buildGiftItem("GIFT_1", 2), buildGiftItem("GIFT_2", 3)));
        giftAct.setRulesJson(JSONUtil.toJsonStr(Collections.singletonList(g1)));

        CartSkuVO sku1 = buildCartSku("SKU_1", 250D, PromotionTypeEnum.FULL_DISCOUNT.name() + "-ACT_MINUS", minusAct);
        CartSkuVO sku2 = buildCartSku("SKU_2", 120D, PromotionTypeEnum.FULL_DISCOUNT.name() + "-ACT_GIFT", giftAct);

        CartVO cartVO = new CartVO();
        cartVO.setStoreId("STORE_1");
        cartVO.setSkuList(Arrays.asList(sku1, sku2));
        cartVO.getPriceDetailDTO().setGoodsPrice(370D);

        TradeDTO tradeDTO = new TradeDTO();
        tradeDTO.setCartTypeEnum(CartTypeEnum.CART);
        tradeDTO.setCartList(Collections.singletonList(cartVO));
        tradeDTO.setSkuList(Arrays.asList(sku1, sku2));

        render.render(tradeDTO);

        assertThat(sku1.getPriceDetailDTO().getDiscountPrice()).isEqualTo(30D);
        assertThat(cartVO.getGiftList()).containsExactly("GIFT_1", "GIFT_2");
        assertThat(cartVO.getGiftItemList()).hasSize(2);
        assertThat(cartVO.getGiftItemList().get(0).getNum()).isEqualTo(2);
        assertThat(cartVO.getGiftItemList().get(1).getNum()).isEqualTo(3);
        assertThat(cartVO.getFullDiscountList()).hasSize(2);
    }

    /**
     * 构造基础满额活动视图对象。
     *
     * @param activityType 活动子类型编码
     * @return 基础满额活动视图对象
     */
    private FullDiscountVO buildBaseVO(String activityType) {
        FullDiscountVO vo = new FullDiscountVO();
        vo.setTitle("测试活动");
        vo.setActivityType(activityType);
        vo.setScopeType(PromotionsScopeTypeEnum.ALL.name());
        vo.setStartTime(new Date(System.currentTimeMillis() - 86_400_000L));
        vo.setEndTime(new Date(System.currentTimeMillis() + 86_400_000L * 30));
        return vo;
    }

    /**
     * 构造赠品配置项。
     *
     * @param skuId 赠品SKU ID
     * @param num   赠送件数
     * @return 赠品配置项
     */
    private FullDiscountGiftItem buildGiftItem(String skuId, int num) {
        FullDiscountGiftItem item = new FullDiscountGiftItem();
        item.setSkuId(skuId);
        item.setNum(num);
        return item;
    }

    /**
     * 构造购物车SKU视图对象。
     *
     * @param skuId        SKU ID
     * @param subTotal     小计金额
     * @param promotionKey 促销Map键
     * @param fullDiscount 满额活动对象
     * @return 购物车SKU视图对象
     */
    private CartSkuVO buildCartSku(String skuId, Double subTotal, String promotionKey, FullDiscount fullDiscount) {
        GoodsSku goodsSku = new GoodsSku();
        goodsSku.setId(skuId);
        goodsSku.setPrice(subTotal);
        CartSkuVO cartSkuVO = new CartSkuVO(goodsSku);
        cartSkuVO.setChecked(true);
        cartSkuVO.setSubTotal(subTotal);
        cartSkuVO.getPriceDetailDTO().setGoodsPrice(subTotal);
        Map<String, Object> promotionMap = new HashMap<>();
        promotionMap.put(promotionKey, fullDiscount);
        cartSkuVO.setPromotionMap(promotionMap);
        return cartSkuVO;
    }

    /**
     * 构造关联指定赠品SKU的满额赠活动。
     *
     * @param giftSkuId 赠品SKU ID
     * @return 关联指定赠品SKU的满额赠活动
     */
    private FullDiscount buildGiftPromotion(String giftSkuId) {
        FullDiscount fullDiscount = new FullDiscount();
        fullDiscount.setGiftFlag(true);
        fullDiscount.setGiftId(giftSkuId);
        fullDiscount.setFullMoney(100D);
        fullDiscount.setTitle("满100赠礼");
        fullDiscount.setStartTime(new Date(System.currentTimeMillis() - 86_400_000L));
        fullDiscount.setEndTime(new Date(System.currentTimeMillis() + 86_400_000L * 30));
        return fullDiscount;
    }

    /**
     * 构造指定商品类型的赠品SKU。
     *
     * @param skuId     SKU ID
     * @param goodsType 商品类型
     * @return 指定商品类型的赠品SKU
     */
    private GoodsSku buildGiftSku(String skuId, GoodsTypeEnum goodsType) {
        GoodsSku giftSku = new GoodsSku();
        giftSku.setId(skuId);
        giftSku.setGoodsName("商品-" + skuId);
        giftSku.setPrice(10D);
        giftSku.setGoodsType(goodsType.name());
        return giftSku;
    }
}
