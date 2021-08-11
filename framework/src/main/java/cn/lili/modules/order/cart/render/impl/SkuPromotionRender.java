package cn.lili.modules.order.cart.render.impl;

import cn.hutool.core.date.DateUtil;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.order.cart.entity.dto.MemberCouponDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.order.cart.entity.vo.PriceDetailVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dto.GoodsSkuPromotionPriceDTO;
import cn.lili.modules.promotion.entity.dto.PromotionPriceDTO;
import cn.lili.modules.promotion.entity.dto.PromotionPriceParamDTO;
import cn.lili.modules.promotion.entity.dto.StorePromotionPriceDTO;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.service.PromotionPriceService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * 购物促销信息渲染实现
 *
 * @author Chopper
 * @since 2020-07-02 14:47
 */
@Service
@Order(2)
public class SkuPromotionRender implements CartRenderStep {



    /**
     * 促销计算
     */
    @Autowired
    private PromotionPriceService promotionPriceService;
    /**
     * 促销商品
     */
    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Override
    public RenderStepEnums step() {
        return RenderStepEnums.SKU_PROMOTION;
    }

    @Override
    public void render(TradeDTO tradeDTO) {

        //渲染促销价格
        this.renderPromotionPrice(tradeDTO);

        //获取商品促销
        renderSkuPromotion(tradeDTO);
    }


    /**
     * 渲染单品优惠 积分/拼团/秒杀/砍价
     *
     * @param tradeDTO 购物车视图
     */
    private void renderSkuPromotion(TradeDTO tradeDTO) {

        //非积分商品、拼团、砍价商品可渲染满优惠活动
        //这里普通购物车也只渲染满优惠，其他优惠都是商品级别的，都写在商品属性里
        if (!tradeDTO.getCartTypeEnum().equals(CartTypeEnum.POINTS)
                && !tradeDTO.getCartTypeEnum().equals(CartTypeEnum.PINTUAN)
                && !tradeDTO.getCartTypeEnum().equals(CartTypeEnum.KANJIA)) {
            List<CartVO> cartVOS = tradeDTO.getCartList();
            for (CartVO cartVO : cartVOS) {
                if (isFull(cartVO)) {
                    this.renderFullMinus(cartVO);
                }
                for (CartSkuVO cartSkuVO : cartVO.getSkuList()) {
                    promotionGoodsService.getCartSkuPromotion(cartSkuVO);
                }
            }
        }
    }

    /**
     * 渲染购物车视图的促销价格
     * 1.获取购物车商品列表
     * 2.获取用户的优惠券
     * 3.调用价格计算模块，返回价格计算结果
     * 4.分配计算后的促销
     *
     * @param tradeDTO 购物车视图
     */
    private void renderPromotionPrice(TradeDTO tradeDTO) {

        //获取购物车商品列表
        List<PromotionPriceParamDTO> promotionPriceParamList = this.getPromotionPriceParamList(tradeDTO);
        //店铺优惠券集合
        List<MemberCoupon> memberCoupons = this.getMemberCoupons(tradeDTO);
        //调用价格计算模块，返回价格计算结果
        PromotionPriceDTO promotionPrice = promotionPriceService.calculationPromotionPrice(promotionPriceParamList, memberCoupons,tradeDTO.getCartTypeEnum());
        // 分配计算后的促销
        this.distributionPromotionPrice(tradeDTO, promotionPrice);
    }

    /**
     * 获取用户优惠券列表
     *
     * @param tradeDTO 交易DTO
     * @return 用户优惠券列表
     */
    private List<MemberCoupon> getMemberCoupons(TradeDTO tradeDTO) {
        //店铺优惠券集合
        List<MemberCoupon> memberCoupons = new ArrayList<>();
        if (tradeDTO.getStoreCoupons() != null) {
            memberCoupons.addAll(tradeDTO.getStoreCoupons().values().parallelStream().map(MemberCouponDTO::getMemberCoupon).collect(Collectors.toList()));
        }

        //平台优惠券
        if (tradeDTO.getPlatformCoupon() != null && tradeDTO.getPlatformCoupon().getMemberCoupon() != null) {
            memberCoupons.add(tradeDTO.getPlatformCoupon().getMemberCoupon());
        }

        //清除过期优惠券
        long now = DateUtil.date().getTime();
        memberCoupons.removeIf(memberCoupon -> memberCoupon.getEndTime().getTime() < now);
        return memberCoupons;
    }

    /**
     * 获取促销价格DTO列表
     *
     * @param tradeDTO 交易DTO
     * @return 促销价格DTO列表
     */
    private List<PromotionPriceParamDTO> getPromotionPriceParamList(TradeDTO tradeDTO) {
        List<PromotionPriceParamDTO> promotionPriceParamList = new ArrayList<>();
        List<CartVO> cartList = tradeDTO.getCartList();
        for (CartVO cartVO : cartList) {
            if (Boolean.TRUE.equals(cartVO.getChecked())) {
                for (CartSkuVO cartSkuVO : cartVO.getSkuList()) {
                    //检查当前购物车商品是否有效且为选中
                    if (Boolean.TRUE.equals(cartSkuVO.getChecked()) && Boolean.FALSE.equals(cartSkuVO.getInvalid())) {
                        PromotionPriceParamDTO param = new PromotionPriceParamDTO();
                        param.setSkuId(cartSkuVO.getGoodsSku().getId());
                        param.setNum(cartSkuVO.getNum());
                        //是否为拼团商品计算
                        if (cartSkuVO.getPintuanId() != null) {
                            param.setPintuanId(cartSkuVO.getPintuanId());
                        }
                        promotionPriceParamList.add(param);
                    }
                }
            }
        }
        return promotionPriceParamList;
    }

    /**
     * 分配促销价格到购物车视图
     *
     * @param tradeDTO       购物车视图
     * @param promotionPrice 促销价格计算结果
     */
    private void distributionPromotionPrice(TradeDTO tradeDTO, PromotionPriceDTO promotionPrice) {

        for (CartVO cartVO : tradeDTO.getCartList()) {
            //根据店铺分配店铺价格计算结果
            Optional<StorePromotionPriceDTO> storePromotionPriceDTOOptional = promotionPrice.getStorePromotionPriceList().parallelStream().filter(i -> i.getStoreId().equals(cartVO.getStoreId())).findAny();
            if (storePromotionPriceDTOOptional.isPresent()) {
                StorePromotionPriceDTO storePromotionPriceDTO = storePromotionPriceDTOOptional.get();
                //根据商品分配商品结果计算结果
                this.distributionSkuPromotionPrice(cartVO.getSkuList(), storePromotionPriceDTO);

                PriceDetailDTO sSpd = new PriceDetailDTO();
                PriceDetailVO sPd = new PriceDetailVO();
                sSpd.setGoodsPrice(storePromotionPriceDTO.getTotalOriginPrice());
                sSpd.setDiscountPrice(storePromotionPriceDTO.getTotalDiscountPrice());
                sSpd.setCouponPrice(storePromotionPriceDTO.getTotalCouponPrice());
                sSpd.setPayPoint(storePromotionPriceDTO.getTotalPoints().intValue());

                sPd.setOriginalPrice(storePromotionPriceDTO.getTotalOriginPrice());
                sPd.setFinalePrice(storePromotionPriceDTO.getTotalFinalePrice());
                sPd.setDiscountPrice(storePromotionPriceDTO.getTotalDiscountPrice());
                sPd.setPayPoint(storePromotionPriceDTO.getTotalPoints().intValue());
                cartVO.setPriceDetailDTO(sSpd);
                cartVO.setPriceDetailVO(sPd);
                cartVO.setWeight(storePromotionPriceDTO.getTotalWeight());
            }
        }

        //根据整个购物车分配价格计算结果
        PriceDetailDTO priceDetailDTO = new PriceDetailDTO();

        priceDetailDTO.setDiscountPrice(promotionPrice.getTotalDiscountPrice());
        priceDetailDTO.setGoodsPrice(promotionPrice.getTotalOriginPrice());
        priceDetailDTO.setCouponPrice(promotionPrice.getTotalCouponPrice());
        priceDetailDTO.setPayPoint(promotionPrice.getTotalPoints().intValue());

        tradeDTO.setPriceDetailDTO(priceDetailDTO);
    }

    /**
     * 分配促销价格到购物车视图的每个sku
     *
     * @param skuList                sku列表
     * @param storePromotionPriceDTO 店铺促销结果计算结果
     */
    private void distributionSkuPromotionPrice(List<CartSkuVO> skuList, StorePromotionPriceDTO storePromotionPriceDTO) {
        if (storePromotionPriceDTO != null) {
            for (CartSkuVO cartSkuVO : skuList) {
                //获取当前购物车商品的商品计算结果
                List<GoodsSkuPromotionPriceDTO> collect = storePromotionPriceDTO.getGoodsSkuPromotionPriceList().parallelStream().filter(i -> i.getSkuId().equals(cartSkuVO.getGoodsSku().getId())).collect(Collectors.toList());
                if (!collect.isEmpty()) {
                    GoodsSkuPromotionPriceDTO goodsSkuPromotionPriceDTO = collect.get(0);
                    PriceDetailDTO spd = new PriceDetailDTO();
                    spd.setDiscountPrice(goodsSkuPromotionPriceDTO.getTotalDiscountPrice());
                    spd.setGoodsPrice(goodsSkuPromotionPriceDTO.getTotalOriginalPrice());
                    spd.setCouponPrice(goodsSkuPromotionPriceDTO.getCouponPrice());
                    spd.setJoinPromotion(goodsSkuPromotionPriceDTO.getJoinPromotion());
                    spd.setPayPoint(goodsSkuPromotionPriceDTO.getTotalPoints().intValue());
                    PriceDetailVO pd = new PriceDetailVO();
                    pd.setFinalePrice(goodsSkuPromotionPriceDTO.getFinalePrice());
                    pd.setOriginalPrice(goodsSkuPromotionPriceDTO.getOriginalPrice());
                    pd.setDiscountPrice(goodsSkuPromotionPriceDTO.getDiscountPrice());
                    pd.setPayPoint(goodsSkuPromotionPriceDTO.getTotalPoints().intValue());
                    cartSkuVO.setPriceDetailDTO(spd);
                    cartSkuVO.setPriceDetailVO(pd);
                }

            }
        }

    }

    /**
     * 渲染满减优惠
     *
     * @param cartVO 购物车展示信息
     */
    private void renderFullMinus(CartVO cartVO) {

        //获取参与活动的商品总价
        FullDiscountVO fullDiscount = cartVO.getFullDiscount();

        if (Boolean.TRUE.equals(fullDiscount.getIsCoupon())) {
            cartVO.getGiftCouponList().add(fullDiscount.getCouponId());
        }
        if (Boolean.TRUE.equals(fullDiscount.getIsGift())) {
            cartVO.setGiftList(Arrays.asList(fullDiscount.getGiftId().split(",")));
        }
        if (Boolean.TRUE.equals(fullDiscount.getIsPoint())) {
            cartVO.setGiftPoint(cartVO.getGiftPoint() + fullDiscount.getPoint());
        }

    }


    /**
     * 是否满足满优惠
     *
     * @param cart 购物车展示信息
     * @return 是否满足满优惠
     */
    private boolean isFull(CartVO cart) {
        Double price = cart.getPriceDetailDTO().getGoodsPrice();
        if (cart.getFullDiscount() != null) {
            if (cart.getFullDiscount().getFullMoney() <= price) {
                cart.setPromotionNotice("正在参与满优惠活动（" + cart.getFullDiscount().getPromotionName() + "）" + cart.getFullDiscount().notice());
                //如果满足，判定是否免邮，免邮的话需要渲染一边sku
                if (Boolean.TRUE.equals(cart.getFullDiscount().getIsFreeFreight())) {
                    for (CartSkuVO skuVO : cart.getSkuList()) {
                        skuVO.setIsFreeFreight(true);
                    }
                }
                return true;
            } else {
                cart.setPromotionNotice("还差" + CurrencyUtil.sub(cart.getFullDiscount().getFullMoney(), price) + " 即可参与活动（" + cart.getFullDiscount().getPromotionName() + "）" + cart.getFullDiscount().notice());
                return false;
            }
        } else {
            cart.setPromotionNotice("");
        }
        return false;
    }

}
