package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Date;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.when;

/**
 * 满额活动赠品 SKU 校验（含 E_COUPON）。
 */
class FullDiscountServiceImplGiftValidationTest {

    private FullDiscountServiceImpl fullDiscountService;
    private GoodsSkuService goodsSkuService;

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        fullDiscountService = spy(new FullDiscountServiceImpl());
        goodsSkuService = mock(GoodsSkuService.class);
        ReflectionTestUtils.setField(fullDiscountService, "goodsSkuService", goodsSkuService);
        doReturn(true).when(fullDiscountService).allowExistSame();
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
        giftSku.setGoodsType(goodsType.name());
        return giftSku;
    }
}
