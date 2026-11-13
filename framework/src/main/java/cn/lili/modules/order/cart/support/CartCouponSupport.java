package cn.lili.modules.order.cart.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.cart.entity.dto.MemberCouponDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dto.search.MemberCouponSearchParams;
import cn.lili.modules.promotion.entity.enums.MemberCouponStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import cn.lili.modules.search.service.GoodsSearchService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 购物车优惠券支持类，负责可用优惠券数量统计、优惠券选中/取消及适用商品范围与门槛校验。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class CartCouponSupport {

    @Autowired
    private MemberCouponService memberCouponService;

    @Autowired
    private GoodsSearchService goodsSearchService;

    @Autowired
    private GoodsSkuService goodsSkuService;

    public CartCouponSupport() {
    }

    public CartCouponSupport(MemberCouponService memberCouponService,
                             GoodsSearchService goodsSearchService,
                             GoodsSkuService goodsSkuService) {
        this.memberCouponService = memberCouponService;
        this.goodsSearchService = goodsSearchService;
        this.goodsSkuService = goodsSkuService;
    }

    /**
     * 统计当前购物车内可使用的优惠券数量（包含指定商品券及全品类/店铺门槛券）。
     *
     * @param tradeDTO 当前交易购物车上下文
     * @return 可使用的优惠券总数
     */
    public Long getCanUseCoupon(TradeDTO tradeDTO) {
        long count = 0L;
        double totalPrice = tradeDTO.getSkuList().stream().mapToDouble(i -> i.getPurchasePrice() * i.getNum()).sum();
        if (tradeDTO.getSkuList() != null && !tradeDTO.getSkuList().isEmpty()) {
            List<String> ids = tradeDTO.getSkuList().stream()
                    .filter(i -> Boolean.TRUE.equals(i.getChecked()))
                    .map(i -> i.getGoodsSku().getId())
                    .collect(Collectors.toList());

            List<GoodsIndex> goodsIndexList = goodsSearchService.getGoodsBySkuIds(ids, null);
            for (GoodsIndex goodsIndex : goodsIndexList) {
                if (goodsIndex != null && goodsIndex.getPromotionMap() != null && !goodsIndex.getPromotionMap().isEmpty()) {
                    List<String> couponIds = goodsIndex.getPromotionMap().keySet().stream()
                            .filter(i -> i.contains(PromotionTypeEnum.COUPON.name()))
                            .map(i -> i.substring(i.lastIndexOf("-") + 1))
                            .collect(Collectors.toList());
                    if (!couponIds.isEmpty()) {
                        List<MemberCoupon> currentGoodsCanUse = memberCouponService.getCurrentGoodsCanUse(tradeDTO.getMemberId(), couponIds, totalPrice);
                        count = currentGoodsCanUse.size();
                    }
                }
            }

            List<String> storeIds = new ArrayList<>();
            for (CartSkuVO cartSkuVO : tradeDTO.getSkuList()) {
                if (!storeIds.contains(cartSkuVO.getStoreId())) {
                    storeIds.add(cartSkuVO.getStoreId());
                }
            }

            // 获取可操作的优惠券集合
            List<MemberCoupon> allScopeMemberCoupon = memberCouponService.getAllScopeMemberCoupon(tradeDTO.getMemberId(), storeIds);
            if (allScopeMemberCoupon != null && !allScopeMemberCoupon.isEmpty()) {
                // 过滤满足消费门槛
                count += allScopeMemberCoupon.stream().filter(i -> i.getConsumeThreshold() <= totalPrice).count();
            }
        }
        return count;
    }

    /**
     * 在购物车中选中或取消使用指定会员优惠券。
     *
     * @param tradeDTO     交易购物车上下文
     * @param memberId     当前登录会员ID
     * @param couponId     会员优惠券ID
     * @param use          true-使用，false-取消
     * @param cartTypeEnum 购物车类型
     */
    public void selectCoupon(TradeDTO tradeDTO, String memberId, String couponId, boolean use, CartTypeEnum cartTypeEnum) {
        MemberCouponSearchParams searchParams = new MemberCouponSearchParams();
        searchParams.setMemberCouponStatus(MemberCouponStatusEnum.NEW.name());
        searchParams.setMemberId(memberId);
        searchParams.setId(couponId);
        MemberCoupon memberCoupon = memberCouponService.getMemberCoupon(searchParams);
        if (memberCoupon == null) {
            throw new ServiceException(ResultCode.COUPON_EXPIRED);
        }
        if (use) {
            this.useCoupon(tradeDTO, memberCoupon, cartTypeEnum);
        } else {
            if (Boolean.TRUE.equals(memberCoupon.getPlatformFlag())) {
                tradeDTO.setPlatformCoupon(null);
            } else {
                tradeDTO.getStoreCoupons().remove(memberCoupon.getStoreId());
            }
        }
    }

    /**
     * 校验优惠券门槛并将优惠券应用到对应平台或店铺槽位。
     *
     * @param tradeDTO     交易对象
     * @param memberCoupon 客户优惠券
     * @param cartTypeEnum 购物车类型
     */
    private void useCoupon(TradeDTO tradeDTO, MemberCoupon memberCoupon, CartTypeEnum cartTypeEnum) {
        // 截取符合优惠券的商品
        List<CartSkuVO> cartSkuVOS = checkCoupon(memberCoupon, tradeDTO);

        // 定义使用优惠券的商品金额映射
        Map<String, Double> skuPrice = new HashMap<>(1);
        double cartPrice = 0d;

        for (CartSkuVO cartSkuVO : cartSkuVOS) {
            if (Boolean.FALSE.equals(cartSkuVO.getChecked())) {
                continue;
            }
            // 使用优惠券时判断最新的sku价格，与后续渲染保持一致
            GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(cartSkuVO.getGoodsSku().getId());
            if (cartSkuVO.getPromotionMap() != null && !cartSkuVO.getPromotionMap().isEmpty()
                    && cartSkuVO.getPromotionMap().keySet().stream().anyMatch(
                    i -> i.contains(PromotionTypeEnum.PINTUAN.name()) || i.contains(PromotionTypeEnum.SECKILL.name()))) {
                double amount = CurrencyUtil.mul(cartSkuVO.getPurchasePrice(), cartSkuVO.getNum());
                cartPrice = CurrencyUtil.add(cartPrice, amount);
                skuPrice.put(cartSkuVO.getGoodsSku().getId(), amount);
            } else {
                double amount = CurrencyUtil.mul(goodsSku.getPrice(), cartSkuVO.getNum());
                cartPrice = CurrencyUtil.add(cartPrice, amount);
                skuPrice.put(cartSkuVO.getGoodsSku().getId(), amount);
            }
        }

        // 如果符合条件商品金额大于等于消费门槛则应用优惠券
        if (cartPrice >= memberCoupon.getConsumeThreshold()) {
            if (Boolean.TRUE.equals(memberCoupon.getPlatformFlag())) {
                tradeDTO.setPlatformCoupon(new MemberCouponDTO(skuPrice, memberCoupon));
            } else {
                tradeDTO.getStoreCoupons().put(memberCoupon.getStoreId(), new MemberCouponDTO(skuPrice, memberCoupon));
            }
        }
    }

    /**
     * 筛选购物车中满足当前优惠券适用范围的商品SKU列表。
     *
     * @param memberCoupon 客户优惠券
     * @param tradeDTO     购物车信息
     * @return 符合优惠券适用范围的SKU列表
     */
    private List<CartSkuVO> checkCoupon(MemberCoupon memberCoupon, TradeDTO tradeDTO) {
        List<CartSkuVO> cartSkuVOS;
        if (Boolean.FALSE.equals(memberCoupon.getPlatformFlag())) {
            cartSkuVOS = tradeDTO.getSkuList().stream()
                    .filter(i -> i.getStoreId().equals(memberCoupon.getStoreId()))
                    .collect(Collectors.toList());
        } else {
            cartSkuVOS = tradeDTO.getSkuList();
        }

        if (memberCoupon.getScopeType().equals(PromotionsScopeTypeEnum.ALL.name())) {
            return cartSkuVOS;
        } else if (memberCoupon.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_GOODS_CATEGORY.name())) {
            return cartSkuVOS.stream()
                    .filter(i -> CharSequenceUtil.contains(memberCoupon.getScopeId(), i.getGoodsSku().getCategoryPath()))
                    .collect(Collectors.toList());
        } else if (memberCoupon.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_GOODS.name())) {
            return cartSkuVOS.stream()
                    .filter(i -> CharSequenceUtil.contains(memberCoupon.getScopeId(), i.getGoodsSku().getId()))
                    .collect(Collectors.toList());
        } else if (memberCoupon.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_SHOP_CATEGORY.name())) {
            return cartSkuVOS.stream()
                    .filter(i -> CharSequenceUtil.contains(memberCoupon.getScopeId(), i.getGoodsSku().getStoreCategoryPath()))
                    .collect(Collectors.toList());
        }
        return new ArrayList<>();
    }
}
