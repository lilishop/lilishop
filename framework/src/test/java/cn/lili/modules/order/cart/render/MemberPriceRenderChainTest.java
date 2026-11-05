package cn.lili.modules.order.cart.render;

import cn.lili.cache.Cache;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.PaidMemberOrder;
import cn.lili.modules.member.service.PaidMemberService;
import cn.lili.modules.system.entity.dto.PaidMemberSetting;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.render.impl.*;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.test.util.ReflectionTestUtils;
import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Stream;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * 验证真实渲染链先校验、再会员价、后其他促销，并检查会员金额、满减门槛与平台补贴。
 * @author Bulbasaur
 * @since 2026-11-05
 */
class MemberPriceRenderChainTest {
    /** 使用真实执行器，避免只校验数组而漏掉价格覆盖。 */
    private final TradeBuilder builder = new TradeBuilder();
    /** 隔离会员资格查询，价格计算仍使用真实渲染器。 */
    private final PaidMemberService members = mock(PaidMemberService.class);
    /** 隔离当前活动查询。 */
    private final PromotionGoodsService promotions = mock(PromotionGoodsService.class);
    /** 测试交易数据，不预先初始化商品金额明细。 */
    private TradeDTO trade;
    /** 两件原价 100 元的实物商品。 */
    private CartSkuVO item;
    /** 当前参与的活动，供商品校验刷新。 */
    private final Map<String,Object> activity = new HashMap<>();

    /** 初始化校验、会员、单品、满减和汇总的真实渲染器。 */
    @BeforeEach void setup() {
        GoodsSku sku = new GoodsSku(); sku.setId("sku"); sku.setGoodsId("goods"); sku.setStoreId("store");
        sku.setPrice(100D); sku.setQuantity(100); sku.setGoodsType("PHYSICAL_GOODS");
        sku.setAuthFlag("PASS"); sku.setMarketEnable("UPPER"); sku.setPromotionFlag(false);
        item = new CartSkuVO(sku); item.setNum(2); item.setSubTotal(200D); item.setCartType(CartTypeEnum.CART);
        item.setPromotionMap(activity);
        CartVO cart = new CartVO(item); cart.setSkuList(List.of(item));
        trade = new TradeDTO(CartTypeEnum.CART); trade.setMemberId("member");
        trade.setSkuList(new ArrayList<>(List.of(item))); trade.setCartList(new ArrayList<>(List.of(cart)));
        var skuService = mock(GoodsSkuService.class); when(skuService.getGoodsSkuByIdFromCache("sku")).thenReturn(sku);
        when(promotions.getCurrentGoodsPromotion(any(),anyString())).thenReturn(activity);
        var check = new CheckDataRender(); ReflectionTestUtils.setField(check,"goodsSkuService",skuService);
        ReflectionTestUtils.setField(check,"promotionGoodsService",promotions);
        var member = new PaidMemberRender(); ReflectionTestUtils.setField(member,"service",members);
        var promotion = new SkuPromotionRender(); ReflectionTestUtils.setField(promotion,"cache",mock(Cache.class));
        var full = new FullDiscountRender();
        ReflectionTestUtils.setField(builder,"cartRenderSteps",List.of(new CheckedFilterRender(),check,member,promotion,full,new CartPriceRender()));
        var order = new PaidMemberOrder(); order.setId("membership"); order.setConfigVersion("version");
        var config = new PaidMemberSetting(); when(members.active("member")).thenReturn(order);
        when(members.snapshot(order)).thenReturn(config); when(members.discount(config,sku)).thenReturn(new BigDecimal("8.8"));
    }

    /**
     * 返回所有已声明的价格链，防止仅购物车修正而结算或建单仍沿用旧顺序。
     * @return 包含购物车、结算、建单及专用场景的步骤数组
     */
    static Stream<org.junit.jupiter.params.provider.Arguments> chains() {
        return Stream.of(RenderStepStatement.cartRender,RenderStepStatement.checkedRender,
                RenderStepStatement.checkedSingleRender,RenderStepStatement.singleTradeRender,
                RenderStepStatement.pintuanTradeRender,RenderStepStatement.tradeRender,
                RenderStepStatement.eCouponCheckedRender,RenderStepStatement.eCouponTradeRender)
                .map(steps -> org.junit.jupiter.params.provider.Arguments.of((Object) steps));
    }

    /**
     * 每条链经过真实执行器后仍实付 176 元，平台补贴 24 元、结算基数 200 元。
     * @param steps 被验证的渲染链
     */
    @ParameterizedTest @MethodSource("chains")
    void memberPriceRunsImmediatelyAfterValidationAndSurvivesSkuPromotion(RenderStepEnums[] steps) {
        List<RenderStepEnums> sequence = Arrays.asList(steps);
        assertThat(sequence.indexOf(RenderStepEnums.PAID_MEMBER)).isEqualTo(sequence.indexOf(RenderStepEnums.CHECK_DATA)+1);
        assertThat(sequence.indexOf(RenderStepEnums.SKU_PROMOTION)).isGreaterThan(sequence.indexOf(RenderStepEnums.PAID_MEMBER));
        ReflectionTestUtils.invokeMethod(builder,"renderCartBySteps",trade,steps);
        assertThat(item.getPurchasePrice()).isEqualTo(88D);
        assertThat(trade.getPriceDetailDTO().getFlowPrice()).isEqualTo(176D);
        assertThat(trade.getPriceDetailDTO().getBillPrice()).isEqualTo(200D);
        assertThat(trade.getPriceDetailDTO().getPaidMemberSubsidy()).isEqualTo(24D);
        assertThat(item.getPriceDetailDTO().getDiscountPriceDetail()).hasSize(1);
    }

    /**
     * 构造满指定金额减十元活动，触发后续商品促销回填及满减计算。
     * @param threshold 满减门槛，单位元
     */
    private void fullDiscount(double threshold) {
        FullDiscount full = new FullDiscount(); full.setId("full"); full.setFullMoney(threshold);
        full.setFullMinusFlag(true); full.setFullMinus(10D); full.setScopeType("ALL");
        full.setGiftFlag(false); full.setPointFlag(false); full.setCouponFlag(false); full.setFreeFreightFlag(false);
        activity.put("FULL_DISCOUNT-full",full);
    }

    /** 会员价先把金额降至 176 元，原价能达到的满 180 元门槛不应再触发。 */
    @Test void fullDiscountThresholdUsesMemberPrice() {
        fullDiscount(180D);
        ReflectionTestUtils.invokeMethod(builder,"renderCartBySteps",trade,RenderStepStatement.checkedRender);
        assertThat(trade.getPriceDetailDTO().getFlowPrice()).isEqualTo(176D);
        assertThat(item.getPriceDetailDTO().getDiscountPrice()).isEqualTo(24D);
    }

    /** 会员价 176 元可再满 170 减 10，最终实付 166，平台仅承担原会员优惠 24。 */
    @Test void qualifyingFullDiscountStacksAfterMemberPriceWithoutLosingSubsidy() {
        fullDiscount(170D);
        ReflectionTestUtils.invokeMethod(builder,"renderCartBySteps",trade,RenderStepStatement.tradeRender);
        assertThat(trade.getPriceDetailDTO().getFlowPrice()).isEqualTo(166D);
        assertThat(trade.getPriceDetailDTO().getPaidMemberSubsidy()).isEqualTo(24D);
        assertThat(trade.getPriceDetailDTO().getBillPrice()).isEqualTo(190D);
        assertThat(item.getPriceDetailDTO().getDiscountPriceDetail()).hasSize(2);
    }

    /** 普通客户仍保持原商品价格，会员步骤前移不改变非会员价格。 */
    @Test void ordinaryCustomerKeepsRegularPrice() {
        when(members.active("member")).thenReturn(null);
        ReflectionTestUtils.invokeMethod(builder,"renderCartBySteps",trade,RenderStepStatement.checkedRender);
        assertThat(trade.getPriceDetailDTO().getFlowPrice()).isEqualTo(200D);
        assertThat(trade.getPriceDetailDTO().getPaidMemberSubsidy()).isZero();
    }
}
