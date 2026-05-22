package cn.lili.modules.order.cart.render.impl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.promotion.entity.dto.GiftCardCashDeductPreviewDTO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashDeductPreviewVO;
import cn.lili.modules.promotion.service.GiftCardCashService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;

/**
 * 购物车礼品卡抵扣渲染步骤（试算抵扣并将优惠分摊到SKU价格明细）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Service
public class GiftCardRender implements CartRenderStep {

    @Autowired
    private GiftCardCashService giftCardCashService;

    @Override
    public RenderStepEnums step() {
        return RenderStepEnums.GIFT_CARD;
    }

    @Override
    public void render(TradeDTO tradeDTO) {
        // 基础空值保护：没有交易或价格容器时不处理。
        if (tradeDTO == null || tradeDTO.getPriceDetailDTO() == null) {
            return;
        }
        // 每次渲染都回填“当前会员可用礼品卡列表”，用于结算页直接展示。
        tradeDTO.setCanUseGiftCards(giftCardCashService.listAvailableMemberCards(tradeDTO.getMemberId()));
        // 清理上一次渲染残留，确保幂等
        clearSkuGiftCardPrice(tradeDTO);
        List<String> selectedIds = tradeDTO.getSelectedGiftCardIds();
        // 没有已选礼品卡时，统一清空礼品卡相关金额与明细，避免脏数据。
        if (CollUtil.isEmpty(selectedIds)) {
            tradeDTO.setGiftCardDeductTotal(BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));
            tradeDTO.setCashPayTotal(BigDecimal.valueOf(tradeDTO.getPriceDetailDTO().getFlowPrice()).setScale(2, RoundingMode.HALF_UP));
            tradeDTO.setGiftCardDeductItems(new ArrayList<>());
            tradeDTO.getPriceDetailDTO().setGiftCardPrice(0D);
            return;
        }
        // 基于“已勾选SKU当前金额”做礼品卡试算（此时尚未执行CART_PRICE汇总）。
        BigDecimal orderAmount = calculateOrderAmountBeforeCartSummary(tradeDTO);
        // 订单金额小于等于0时无需抵扣，直接清空礼品卡金额。
        if (orderAmount.compareTo(BigDecimal.ZERO) <= 0) {
            tradeDTO.setGiftCardDeductTotal(BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));
            tradeDTO.setCashPayTotal(BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP));
            tradeDTO.setGiftCardDeductItems(new ArrayList<>());
            tradeDTO.getPriceDetailDTO().setGiftCardPrice(0D);
            return;
        }
        // 调用促销域礼品卡试算，得到本次总抵扣、现金差额和逐卡明细。
        GiftCardCashDeductPreviewDTO previewDTO = new GiftCardCashDeductPreviewDTO();
        previewDTO.setCredentialIds(selectedIds);
        previewDTO.setOrderAmount(orderAmount);
        GiftCardCashDeductPreviewVO preview = giftCardCashService.previewMemberCardDeduction(tradeDTO.getMemberId(), previewDTO);
        // 回填交易级结果，供下单与前端展示使用。
        tradeDTO.setGiftCardDeductTotal(preview.getGiftCardDeductAmount());
        tradeDTO.setCashPayTotal(preview.getCashPayAmount());
        tradeDTO.setGiftCardDeductItems(preview.getDeductItems());
        tradeDTO.getPriceDetailDTO().setGiftCardPrice(preview.getGiftCardDeductAmount().doubleValue());
        // 将交易级礼品卡抵扣按SKU金额比例分摊，保证订单项层面的金额可追踪。
        allocateGiftCardToSku(tradeDTO, preview.getGiftCardDeductAmount());
    }

    /**
     * 在CART_PRICE汇总前，按已勾选SKU明细计算当前应付金额。
     */
    private BigDecimal calculateOrderAmountBeforeCartSummary(TradeDTO tradeDTO) {
        BigDecimal orderAmount = BigDecimal.ZERO.setScale(2, RoundingMode.HALF_UP);
        List<CartSkuVO> checkedSkus = tradeDTO.getCheckedSkuList();
        if (CollUtil.isNotEmpty(checkedSkus)) {
            for (CartSkuVO sku : checkedSkus) {
                if (sku.getPriceDetailDTO() == null) {
                    continue;
                }
                orderAmount = orderAmount.add(
                        BigDecimal.valueOf(sku.getPriceDetailDTO().getFlowPrice()).setScale(2, RoundingMode.HALF_UP)
                ).setScale(2, RoundingMode.HALF_UP);
            }
            return orderAmount;
        }
        return BigDecimal.valueOf(tradeDTO.getPriceDetailDTO().getFlowPrice()).setScale(2, RoundingMode.HALF_UP);
    }

    private void clearSkuGiftCardPrice(TradeDTO tradeDTO) {
        for (CartSkuVO sku : tradeDTO.getCheckedSkuList()) {
            if (sku.getPriceDetailDTO() != null) {
                sku.getPriceDetailDTO().setGiftCardPrice(0D);
            }
        }
    }

    /**
     * 将礼品卡抵扣总额按SKU金额比例分摊，尾差归最后一个SKU。
     */
    private void allocateGiftCardToSku(TradeDTO tradeDTO, BigDecimal totalGiftCard) {
        if (totalGiftCard == null || totalGiftCard.compareTo(BigDecimal.ZERO) <= 0) {
            return;
        }
        List<CartSkuVO> checkedSkus = tradeDTO.getCheckedSkuList();
        if (CollUtil.isEmpty(checkedSkus)) {
            return;
        }
        BigDecimal goodsTotal = BigDecimal.ZERO;
        for (CartSkuVO sku : checkedSkus) {
            goodsTotal = goodsTotal.add(BigDecimal.valueOf(sku.getPriceDetailDTO().getGoodsPrice()).setScale(2, RoundingMode.HALF_UP));
        }
        if (goodsTotal.compareTo(BigDecimal.ZERO) <= 0) {
            return;
        }
        BigDecimal assigned = BigDecimal.ZERO;
        for (int i = 0; i < checkedSkus.size(); i++) {
            CartSkuVO sku = checkedSkus.get(i);
            PriceDetailDTO detailDTO = sku.getPriceDetailDTO();
            if (detailDTO == null) {
                continue;
            }
            BigDecimal skuGoods = BigDecimal.valueOf(detailDTO.getGoodsPrice()).setScale(2, RoundingMode.HALF_UP);
            BigDecimal skuGiftCard;
            if (i == checkedSkus.size() - 1) {
                skuGiftCard = totalGiftCard.subtract(assigned).setScale(2, RoundingMode.HALF_UP);
            } else {
                BigDecimal ratio = skuGoods.divide(goodsTotal, 10, RoundingMode.HALF_UP);
                skuGiftCard = totalGiftCard.multiply(ratio).setScale(2, RoundingMode.HALF_UP);
                assigned = assigned.add(skuGiftCard);
            }
            if (skuGiftCard.compareTo(BigDecimal.ZERO) < 0) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "礼品卡分摊金额异常");
            }
            double remainFlow = CurrencyUtil.sub(detailDTO.getFlowPrice(), detailDTO.getGiftCardPrice());
            if (skuGiftCard.doubleValue() > remainFlow) {
                skuGiftCard = BigDecimal.valueOf(remainFlow).setScale(2, RoundingMode.HALF_UP);
            }
            detailDTO.setGiftCardPrice(skuGiftCard.doubleValue());
        }
    }
}
