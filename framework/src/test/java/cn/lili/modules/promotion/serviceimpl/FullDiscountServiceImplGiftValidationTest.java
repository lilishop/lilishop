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

    @BeforeEach
    void setUp() {
        fullDiscountService = spy(new FullDiscountServiceImpl());
        goodsSkuService = mock(GoodsSkuService.class);
        ReflectionTestUtils.setField(fullDiscountService, "goodsSkuService", goodsSkuService);
        doReturn(true).when(fullDiscountService).allowExistSame();
    }

    @Test
    void checkPromotionsAllowsECouponGiftSku() {
        GoodsSku giftSku = buildGiftSku("SKU001", GoodsTypeEnum.E_COUPON);
        when(goodsSkuService.getGoodsSkuByIdFromCache("SKU001")).thenReturn(giftSku);

        assertDoesNotThrow(() -> fullDiscountService.checkPromotions(buildGiftPromotion("SKU001")));
    }

    @Test
    void checkPromotionsAllowsPhysicalGiftSku() {
        GoodsSku giftSku = buildGiftSku("SKU002", GoodsTypeEnum.PHYSICAL_GOODS);
        when(goodsSkuService.getGoodsSkuByIdFromCache("SKU002")).thenReturn(giftSku);

        assertDoesNotThrow(() -> fullDiscountService.checkPromotions(buildGiftPromotion("SKU002")));
    }

    @Test
    void checkPromotionsRejectsMissingGiftSku() {
        when(goodsSkuService.getGoodsSkuByIdFromCache("MISSING")).thenReturn(null);

        ServiceException ex = assertThrows(ServiceException.class,
                () -> fullDiscountService.checkPromotions(buildGiftPromotion("MISSING")));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.FULL_DISCOUNT_GIFT_ERROR);
    }

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

    private GoodsSku buildGiftSku(String skuId, GoodsTypeEnum goodsType) {
        GoodsSku giftSku = new GoodsSku();
        giftSku.setId(skuId);
        giftSku.setGoodsType(goodsType.name());
        return giftSku;
    }
}
