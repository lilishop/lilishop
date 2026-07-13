package cn.lili.modules.order.cart.render.impl;

import com.alibaba.fastjson2.JSON;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import cn.lili.modules.order.cart.render.util.PromotionPriceUtil;
import cn.lili.modules.order.order.entity.dto.DiscountPriceItem;
import cn.lili.modules.promotion.entity.dos.NthItemDiscount;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 第N件优惠计价
 */
@Service
public class NthItemDiscountRender implements CartRenderStep {

    @Override
    public RenderStepEnums step() {
        return RenderStepEnums.NTH_ITEM_DISCOUNT;
    }

    @Override
    public void render(TradeDTO tradeDTO) {
        for (CartVO cart : tradeDTO.getCartList()) {
            Map<String, List<CartSkuVO>> activityGroups = new HashMap<>();
            for (CartSkuVO sku : cart.getCheckedSkuList()) {
                if (sku.getPromotionMap() == null) {
                    continue;
                }
                sku.getPromotionMap().entrySet().stream()
                        .filter(e -> e.getKey().contains(PromotionTypeEnum.NTH_ITEM_DISCOUNT.name()))
                        .findFirst()
                        .ifPresent(entry -> {
                            NthItemDiscount discount = JSON.parseObject(JSON.toJSONString(entry.getValue()), NthItemDiscount.class);
                            activityGroups.computeIfAbsent(discount.getId(), k -> new ArrayList<>()).add(sku);
                        });
            }
            for (Map.Entry<String, List<CartSkuVO>> group : activityGroups.entrySet()) {
                applyNthDiscount(tradeDTO, group.getKey(), group.getValue(), cart);
            }
        }
    }

    private void applyNthDiscount(TradeDTO tradeDTO, String activityId, List<CartSkuVO> skuList, CartVO cart) {
        if (skuList.isEmpty()) {
            return;
        }
        NthItemDiscount discount = JSON.parseObject(
                JSON.toJSONString(skuList.get(0).getPromotionMap().get(PromotionTypeEnum.NTH_ITEM_DISCOUNT.name() + "-" + activityId)),
                NthItemDiscount.class);
        if (discount == null) {
            return;
        }
        int totalNum = skuList.stream().mapToInt(CartSkuVO::getNum).sum();
        int nth = discount.getNthNum() == null ? 2 : discount.getNthNum();
        if (totalNum < nth) {
            return;
        }
        int rewardTimes = totalNum / nth;
        List<Double> unitPrices = new ArrayList<>();
        for (CartSkuVO sku : skuList) {
            double unit = CurrencyUtil.div(sku.getPriceDetailDTO().getGoodsPrice(), sku.getNum());
            for (int i = 0; i < sku.getNum(); i++) {
                unitPrices.add(unit);
            }
        }
        unitPrices.sort(Comparator.naturalOrder());
        double totalDiscount = 0D;
        for (int i = 0; i < rewardTimes; i++) {
            double price = unitPrices.get(i);
            totalDiscount = CurrencyUtil.add(totalDiscount, calcNthDiscount(price, discount));
        }
        if (totalDiscount <= 0) {
            return;
        }
        Map<String, Double> skuPriceDetail = new HashMap<>();
        for (CartSkuVO sku : skuList) {
            skuPriceDetail.put(sku.getGoodsSku().getId(), sku.getPriceDetailDTO().getGoodsPrice());
        }
        PromotionPriceUtil.recountPrice(tradeDTO, skuPriceDetail, totalDiscount, PromotionTypeEnum.NTH_ITEM_DISCOUNT, activityId);
        for (CartSkuVO sku : skuList) {
            if (sku.getPriceDetailDTO().getDiscountPriceDetail() == null) {
                sku.getPriceDetailDTO().setDiscountPriceDetail(new ArrayList<>());
            }
            sku.getPriceDetailDTO().getDiscountPriceDetail().add(
                    DiscountPriceItem.builder()
                            .promotionTypeEnum(PromotionTypeEnum.NTH_ITEM_DISCOUNT)
                            .promotionId(activityId)
                            .skuId(sku.getGoodsSku().getId())
                            .goodsId(sku.getGoodsSku().getGoodsId())
                            .discountPrice(CurrencyUtil.div(totalDiscount, skuList.size()))
                            .build());
        }
    }

    private double calcNthDiscount(double unitPrice, NthItemDiscount discount) {
        String type = discount.getDiscountType() == null ? "HALF" : discount.getDiscountType();
        return switch (type) {
            case "FREE" -> unitPrice;
            case "RATE" -> CurrencyUtil.mul(unitPrice, CurrencyUtil.sub(1, CurrencyUtil.div(discount.getDiscountValue(), 10)));
            default -> CurrencyUtil.div(unitPrice, 2);
        };
    }
}
