package cn.lili.modules.order.cart.support;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.promotion.entity.vos.PointsGoodsVO;
import cn.lili.modules.promotion.service.KanjiaActivityService;
import cn.lili.modules.promotion.service.PointsGoodsService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 购物车加购与活动资格校验支持类单元测试，验证商品状态、卡池/实物库存上限、预约特例及拼团积分限制。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class CartPromotionCheckSupportTest {

    private GoodsSkuService goodsSkuService;
    private CardKeyService cardKeyService;
    private KanjiaActivityService kanjiaActivityService;
    private PointsGoodsService pointsGoodsService;
    private MemberService memberService;
    private CartPromotionCheckSupport checkSupport;

    @BeforeEach
    void setUp() {
        goodsSkuService = mock(GoodsSkuService.class);
        cardKeyService = mock(CardKeyService.class);
        kanjiaActivityService = mock(KanjiaActivityService.class);
        pointsGoodsService = mock(PointsGoodsService.class);
        memberService = mock(MemberService.class);
        checkSupport = new CartPromotionCheckSupport(
                goodsSkuService, cardKeyService, kanjiaActivityService, pointsGoodsService, memberService);
    }

    /**
     * 验证商品不存在或未上架/未审核时抛出 GOODS_NOT_EXIST。
     */
    @Test
    @DisplayName("商品不存在或未审核上架时抛出GOODS_NOT_EXIST")
    void shouldRejectInvalidOrDownGoods() {
        when(goodsSkuService.getGoodsSkuByIdFromCache("MISSING")).thenReturn(null);
        assertThatThrownBy(() -> checkSupport.checkGoods("MISSING"))
                .isInstanceOf(ServiceException.class)
                .extracting(e -> ((ServiceException) e).getResultCode())
                .isEqualTo(ResultCode.GOODS_NOT_EXIST);

        GoodsSku downSku = new GoodsSku();
        downSku.setId("DOWN");
        downSku.setAuthFlag(GoodsAuthEnum.PASS.name());
        downSku.setMarketEnable(GoodsStatusEnum.DOWN.name());
        when(goodsSkuService.getGoodsSkuByIdFromCache("DOWN")).thenReturn(downSku);

        assertThatThrownBy(() -> checkSupport.checkGoods("DOWN"))
                .isInstanceOf(ServiceException.class)
                .extracting(e -> ((ServiceException) e).getResultCode())
                .isEqualTo(ResultCode.GOODS_NOT_EXIST);
    }

    /**
     * 验证预约商品不依赖SKU库存字段并正常设置购买数量。
     */
    @Test
    @DisplayName("预约商品跳过SKU库存校验并保底设置数量为1")
    void shouldHandleAppointmentGoodsQuantityWithoutStockCheck() {
        GoodsSku sku = new GoodsSku();
        sku.setId("APP-1");
        sku.setGoodsType(GoodsTypeEnum.APPOINTMENT_GOODS.name());
        CartSkuVO cartSkuVO = new CartSkuVO(sku, Collections.emptyMap());

        checkSupport.checkSetGoodsQuantity(cartSkuVO, "APP-1", 0);
        assertThat(cartSkuVO.getNum()).isEqualTo(1);

        checkSupport.checkSetGoodsQuantity(cartSkuVO, "APP-1", 3);
        assertThat(cartSkuVO.getNum()).isEqualTo(3);
    }

    /**
     * 验证电子卡券商品按卡池可用数量进行库存校验。
     */
    @Test
    @DisplayName("电子卡券按卡池未使用库存校验，不足时抛出异常")
    void shouldCheckUnusedPoolStockForECoupon() {
        GoodsSku sku = new GoodsSku();
        sku.setId("EC-1");
        sku.setGoodsType(GoodsTypeEnum.E_COUPON.name());
        CartSkuVO cartSkuVO = new CartSkuVO(sku, Collections.emptyMap());

        when(cardKeyService.countUnusedPoolStock("EC-1")).thenReturn(2);
        assertThatThrownBy(() -> checkSupport.checkSetGoodsQuantity(cartSkuVO, "EC-1", 5))
                .isInstanceOf(ServiceException.class)
                .extracting(e -> ((ServiceException) e).getResultCode())
                .isEqualTo(ResultCode.GOODS_SKU_QUANTITY_NOT_ENOUGH);

        checkSupport.checkSetGoodsQuantity(cartSkuVO, "EC-1", 2);
        assertThat(cartSkuVO.getNum()).isEqualTo(2);
    }

    /**
     * 验证普通商品单品最大加购数量限制为99。
     */
    @Test
    @DisplayName("普通实物商品加购超过99件时自动上限截断为99")
    void shouldCapNormalGoodsQuantityAt99() {
        GoodsSku sku = new GoodsSku();
        sku.setId("SKU-1");
        sku.setGoodsType(GoodsTypeEnum.PHYSICAL_GOODS.name());
        CartSkuVO cartSkuVO = new CartSkuVO(sku, Collections.emptyMap());

        when(goodsSkuService.getStock("SKU-1")).thenReturn(500);
        checkSupport.checkSetGoodsQuantity(cartSkuVO, "SKU-1", 120);
        assertThat(cartSkuVO.getNum()).isEqualTo(99);
    }

    /**
     * 验证拼团活动超过限购数量时抛出 CART_PINTUAN_LIMIT_ERROR。
     */
    @Test
    @DisplayName("拼团活动超过限购数抛出CART_PINTUAN_LIMIT_ERROR")
    void shouldThrowWhenPintuanLimitExceeded() {
        GoodsSku sku = new GoodsSku();
        sku.setId("PT-1");
        sku.setGoodsType(GoodsTypeEnum.PHYSICAL_GOODS.name());
        Map<String, Object> promotionMap = Map.of(
                PromotionTypeEnum.PINTUAN.name() + "-1",
                Map.of("id", "PT-ACT-1", "limitNum", 2)
        );
        CartSkuVO cartSkuVO = new CartSkuVO(sku, promotionMap);

        when(goodsSkuService.getStock("PT-1")).thenReturn(10);
        assertThatThrownBy(() -> checkSupport.checkCart(CartTypeEnum.PINTUAN, cartSkuVO, "PT-1", 3))
                .isInstanceOf(ServiceException.class)
                .extracting(e -> ((ServiceException) e).getResultCode())
                .isEqualTo(ResultCode.CART_PINTUAN_LIMIT_ERROR);
    }

    /**
     * 验证积分商品在积分不足时抛出 POINT_NOT_ENOUGH，积分充足时设置兑换所需积分和0元购入价。
     */
    @Test
    @DisplayName("积分商品校验会员积分余额及活动库存并重置购入价")
    void shouldValidateAndPopulatePointsGoods() {
        GoodsSku sku = new GoodsSku();
        sku.setId("POINT-1");
        sku.setGoodsType(GoodsTypeEnum.PHYSICAL_GOODS.name());
        CartSkuVO cartSkuVO = new CartSkuVO(sku, Collections.emptyMap());

        PointsGoodsVO pointsGoodsVO = new PointsGoodsVO();
        pointsGoodsVO.setId("PG-1");
        pointsGoodsVO.setPoints(500L);
        pointsGoodsVO.setActiveStock(10);
        when(goodsSkuService.getStock("POINT-1")).thenReturn(10);
        when(pointsGoodsService.getPointsGoodsDetailBySkuId("POINT-1")).thenReturn(pointsGoodsVO);

        Member poorMember = new Member();
        poorMember.setPoint(100L);
        when(memberService.getUserInfo()).thenReturn(poorMember);
        assertThatThrownBy(() -> checkSupport.checkCart(CartTypeEnum.POINTS, cartSkuVO, "POINT-1", 1))
                .isInstanceOf(ServiceException.class)
                .extracting(e -> ((ServiceException) e).getResultCode())
                .isEqualTo(ResultCode.POINT_NOT_ENOUGH);

        Member richMember = new Member();
        richMember.setPoint(1000L);
        when(memberService.getUserInfo()).thenReturn(richMember);
        checkSupport.checkCart(CartTypeEnum.POINTS, cartSkuVO, "POINT-1", 1);
        assertThat(cartSkuVO.getPoint()).isEqualTo(500L);
        assertThat(cartSkuVO.getPurchasePrice()).isEqualTo(0D);
        assertThat(cartSkuVO.getPointsId()).isEqualTo("PG-1");
    }
}
