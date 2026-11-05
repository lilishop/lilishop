package cn.lili.modules.member.util;
import cn.lili.modules.member.entity.dos.PaidMemberOrder;
import cn.lili.modules.system.entity.dto.PaidMemberSetting;
import cn.lili.modules.member.service.PaidMemberService;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.render.impl.PaidMemberRender;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;
import java.math.BigDecimal;
import java.util.List;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;
/**
 * 会员价资格、活动互斥和订单平台优惠快照。
 * @author Bulbasaur
 * @since 2026-11-05
 */
class PaidMemberRenderTest {
    /**
     * 被测试的会员价渲染器。
     */
    private final PaidMemberRender render=new PaidMemberRender();
    /**
     * 模拟资格及折扣，隔离价格渲染行为。
     */
    private final PaidMemberService members=mock(PaidMemberService.class);
    /**
     * 用于原地渲染的交易样本。
     */
    private final TradeDTO trade=new TradeDTO();
    /**
     * 两件同款商品的购物车项，供金额及快照断言使用。
     */
    private final CartSkuVO item=new CartSkuVO();
    /**
     * 准备两件单价 100 元商品、有效会员及八八折快照。
     */
    @BeforeEach void setup(){
        ReflectionTestUtils.setField(render,"service",members);
        var sku=new GoodsSku();sku.setId("sku");sku.setGoodsId("goods");sku.setPrice(100D);
        item.setGoodsSku(sku);item.setChecked(true);item.setNum(2);item.setPurchasePrice(100D);item.setUtilPrice(100D);item.setSubTotal(200D);
        var price=new PriceDetailDTO();price.setOriginalPrice(200D);price.setGoodsPrice(200D);item.setPriceDetailDTO(price);
        var cart=new CartVO();cart.setSkuList(List.of(item));trade.setCartList(List.of(cart));trade.setMemberId("member");trade.setCartTypeEnum(CartTypeEnum.CART);
        var order=new PaidMemberOrder();order.setId("paid-order");order.setConfigVersion("snapshot-version");when(members.active("member")).thenReturn(order);
        var config=new PaidMemberSetting();when(members.snapshot(order)).thenReturn(config);when(members.discount(config,sku)).thenReturn(new BigDecimal("8.8"));
    }
    /**
     * 验证实付 176 元、平台补贴 24 元、结算基数 200 元，不保存冗余会员来源字段。
     */
    /**
     * 模拟无有效会员，验证保持原价且不产生会员补贴。
     */
    /**
     * 验证拼团专用订单跳过会员折扣，且不查询会员资格。
     */
    /**
     * 验证已有 70 元单品活动价时不叠加会员折扣。
     */
    @Test void validMemberGetsUnitPriceAndPlatformSnapshot(){render.render(trade);assertThat(item.getPurchasePrice()).isEqualTo(88D);assertThat(item.getUtilPrice()).isEqualTo(88D);assertThat(item.getSubTotal()).isEqualTo(176D);var p=item.getPriceDetailDTO();assertThat(p.getPaidMemberSubsidy()).isEqualTo(24D);assertThat(p.getFlowPrice()).isEqualTo(176D);assertThat(p.getBillPrice()).isEqualTo(200D);}
    @Test void expiredOrMissingMembershipKeepsOrdinaryPrice(){when(members.active("member")).thenReturn(null);render.render(trade);assertThat(item.getPurchasePrice()).isEqualTo(100D);assertThat(item.getPriceDetailDTO().getPaidMemberSubsidy()).isZero();}
    @Test void dedicatedActivityOrdersNeverUseMemberDiscount(){trade.setCartTypeEnum(CartTypeEnum.PINTUAN);render.render(trade);assertThat(item.getPurchasePrice()).isEqualTo(100D);verify(members,never()).active(anyString());}
    @Test void existingSingleItemPromotionIsNotStacked(){item.setPurchasePrice(70D);render.render(trade);assertThat(item.getPurchasePrice()).isEqualTo(70D);assertThat(item.getPriceDetailDTO().getPaidMemberSubsidy()).isZero();}
}
