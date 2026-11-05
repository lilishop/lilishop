package cn.lili.modules.order.cart.render.impl;
import cn.lili.modules.member.service.PaidMemberService;
import cn.lili.modules.member.util.PaidMemberRules;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.render.CartRenderStep;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import java.math.BigDecimal;
/**
 * 普通购物车及立即购买订单的实物商品会员价渲染，优惠由平台承担；活动专用订单不叠加。
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Service
public class PaidMemberRender implements CartRenderStep {
    /** 提供会员资格与成交快照折扣规则。 */
    @Autowired private PaidMemberService service;

    /**
     * 商品校验后、商品促销前执行会员价计算。
     * @return 付费会员渲染阶段
     */
    @Override public RenderStepEnums step() { return RenderStepEnums.PAID_MEMBER; }
    /**
     * 先以原单价初始化金额，再计算单件会员价和平台补贴，后续促销在会员价基础上继续计算。
     * 仅处理购物车及立即购买；校验阶段已确定的单品活动价不叠加会员折扣。
     * @param trade 需要原地更新的交易报价
     */
    @Override public void render(TradeDTO trade) {
        if (trade.getCartTypeEnum() != CartTypeEnum.CART && trade.getCartTypeEnum() != CartTypeEnum.BUY_NOW) return;
        var membership = service.active(trade.getMemberId());
        if (membership == null) return;
        var config = service.snapshot(membership);
        for (var cart : trade.getCartList()) for (var item : cart.getCheckedSkuList()) {
            var sku = item.getGoodsSku();
            // 商品已有单品活动价时不叠加；满减和券由后续渲染继续处理。
            if (item.getPurchasePrice() == null || BigDecimal.valueOf(item.getPurchasePrice()).compareTo(BigDecimal.valueOf(sku.getPrice())) != 0) continue;
            var rate = service.discount(config, sku);
            if (rate == null || rate.compareTo(BigDecimal.TEN) >= 0) continue;
            var unit = PaidMemberRules.price(BigDecimal.valueOf(sku.getPrice()), rate);
            var subsidy = BigDecimal.valueOf(sku.getPrice()).subtract(unit).multiply(BigDecimal.valueOf(item.getNum()));
            if (subsidy.signum() <= 0) continue;
            var price = item.getPriceDetailDTO();
            // 会员步骤先于商品促销执行，不能依赖后者初始化 goodsPrice，否则首次渲染金额为零。
            var original = BigDecimal.valueOf(sku.getPrice()).multiply(BigDecimal.valueOf(item.getNum()));
            price.setOriginalPrice(original.doubleValue());
            price.setGoodsPrice(original.doubleValue());
            price.setPaidMemberSubsidy(subsidy.doubleValue());
            item.setPurchasePrice(unit.doubleValue()); item.setSubTotal(unit.multiply(BigDecimal.valueOf(item.getNum())).doubleValue());
            item.setUtilPrice(unit.doubleValue());
            price.setDiscountPrice(cn.lili.common.utils.CurrencyUtil.add(price.getDiscountPrice(), subsidy.doubleValue()));
            price.getDiscountPriceDetail().add(cn.lili.modules.order.order.entity.dto.DiscountPriceItem.builder()
                    .discountPrice(subsidy.doubleValue()).promotionId(membership.getId())
                    .promotionTypeEnum(cn.lili.common.enums.PromotionTypeEnum.PAID_MEMBER)
                    .skuId(sku.getId()).goodsId(sku.getGoodsId()).build());
        }
    }
}
