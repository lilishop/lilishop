package cn.lili.modules.order.cart.support;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
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
import cn.lili.modules.promotion.entity.dos.KanjiaActivity;
import cn.lili.modules.promotion.entity.dto.search.KanjiaActivitySearchParams;
import cn.lili.modules.promotion.entity.enums.KanJiaStatusEnum;
import cn.lili.modules.promotion.entity.vos.PointsGoodsVO;
import cn.lili.modules.promotion.service.KanjiaActivityService;
import cn.lili.modules.promotion.service.PointsGoodsService;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.Optional;

/**
 * 购物车加购与活动资格校验支持类，负责商品上下架状态验证、库存数量上限校验，
 * 以及拼团限购、砍价达成、积分商品余额等营销活动准入检查。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class CartPromotionCheckSupport {

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    private CardKeyService cardKeyService;

    @Autowired
    private KanjiaActivityService kanjiaActivityService;

    @Autowired
    private PointsGoodsService pointsGoodsService;

    @Autowired
    private MemberService memberService;

    public CartPromotionCheckSupport() {
    }

    public CartPromotionCheckSupport(GoodsSkuService goodsSkuService,
                                     CardKeyService cardKeyService,
                                     KanjiaActivityService kanjiaActivityService,
                                     PointsGoodsService pointsGoodsService,
                                     MemberService memberService) {
        this.goodsSkuService = goodsSkuService;
        this.cardKeyService = cardKeyService;
        this.kanjiaActivityService = kanjiaActivityService;
        this.pointsGoodsService = pointsGoodsService;
        this.memberService = memberService;
    }

    /**
     * 校验商品SKU是否存在、是否通过审核及处于上架状态。
     *
     * @param skuId 商品skuId
     * @return 校验通过的商品SKU信息
     */
    public GoodsSku checkGoods(String skuId) {
        GoodsSku dataSku = this.goodsSkuService.getGoodsSkuByIdFromCache(skuId);
        if (dataSku == null) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        if (!GoodsAuthEnum.PASS.name().equals(dataSku.getAuthFlag())
                || !GoodsStatusEnum.UPPER.name().equals(dataSku.getMarketEnable())) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        return dataSku;
    }

    /**
     * 检查并设置购物车商品数量。
     * E_COUPON 库存以卡池 UNUSED 数为准；预约商品库存按预约时段校验，不看 SKU quantity。
     *
     * @param cartSkuVO 购物车SKU视图对象
     * @param skuId     商品SKU ID
     * @param num       购买数量
     */
    public void checkSetGoodsQuantity(CartSkuVO cartSkuVO, String skuId, Integer num) {
        if (cartSkuVO.getGoodsSku() != null
                && GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(cartSkuVO.getGoodsSku().getGoodsType())) {
            int buyNum = num == null || num < 1 ? 1 : num;
            cartSkuVO.setNum(buyNum);
            return;
        }
        int enableStock;
        if (cartSkuVO.getGoodsSku() != null
                && GoodsTypeEnum.E_COUPON.name().equals(cartSkuVO.getGoodsSku().getGoodsType())) {
            enableStock = cardKeyService.countUnusedPoolStock(skuId);
        } else {
            enableStock = goodsSkuService.getStock(skuId);
        }

        if (enableStock <= 0 || enableStock < num) {
            throw new ServiceException(ResultCode.GOODS_SKU_QUANTITY_NOT_ENOUGH);
        }

        if (enableStock <= num) {
            cartSkuVO.setNum(enableStock);
        } else {
            cartSkuVO.setNum(num);
        }

        if (cartSkuVO.getNum() > 99) {
            cartSkuVO.setNum(99);
        }
    }

    /**
     * 校验即时购买/特定活动模式下的购物车商品数量及对应活动资格（拼团/砍价/积分）。
     *
     * @param cartTypeEnum 购物车购买模式枚举
     * @param cartSkuVO    购物车SKU对象
     * @param skuId        商品SKU ID
     * @param num          购买数量
     */
    public void checkCart(CartTypeEnum cartTypeEnum, CartSkuVO cartSkuVO, String skuId, Integer num) {
        this.checkSetGoodsQuantity(cartSkuVO, skuId, num);
        if (cartTypeEnum.equals(CartTypeEnum.PINTUAN)) {
            checkPintuan(cartSkuVO);
        } else if (cartTypeEnum.equals(CartTypeEnum.KANJIA)) {
            checkKanjia(cartSkuVO);
        } else if (cartTypeEnum.equals(CartTypeEnum.POINTS)) {
            checkPoint(cartSkuVO);
        }
    }

    /**
     * 校验拼团活动限购数量并记录拼团ID。
     *
     * @param cartSkuVO 购物车SKU信息
     */
    private void checkPintuan(CartSkuVO cartSkuVO) {
        if (cartSkuVO.getPromotionMap() != null && !cartSkuVO.getPromotionMap().isEmpty()) {
            Optional<Map.Entry<String, Object>> pintuanPromotions = cartSkuVO.getPromotionMap().entrySet().stream()
                    .filter(i -> i.getKey().contains(PromotionTypeEnum.PINTUAN.name()))
                    .findFirst();
            if (pintuanPromotions.isPresent()) {
                JSONObject promotionsObj = JSON.parseObject(JSON.toJSONString(pintuanPromotions.get().getValue()));
                cartSkuVO.setPintuanId(promotionsObj.get("id").toString());
                Integer limitNum = promotionsObj.getInteger("limitNum");
                if (limitNum != 0 && cartSkuVO.getNum() > limitNum) {
                    throw new ServiceException(ResultCode.CART_PINTUAN_LIMIT_ERROR);
                }
            }
        }
    }

    /**
     * 校验当前会员参与的砍价活动是否已成功达成。
     *
     * @param cartSkuVO 购物车SKU信息
     */
    private void checkKanjia(CartSkuVO cartSkuVO) {
        if (cartSkuVO.getPromotionMap() != null && !cartSkuVO.getPromotionMap().isEmpty()) {
            Optional<Map.Entry<String, Object>> kanjiaPromotions = cartSkuVO.getPromotionMap().entrySet().stream()
                    .filter(i -> i.getKey().contains(PromotionTypeEnum.KANJIA.name()))
                    .findFirst();
            if (kanjiaPromotions.isPresent()) {
                JSONObject promotionsObj = JSON.parseObject(JSON.toJSONString(kanjiaPromotions.get().getValue()));
                KanjiaActivitySearchParams kanjiaActivitySearchParams = new KanjiaActivitySearchParams();
                kanjiaActivitySearchParams.setKanjiaActivityGoodsId(promotionsObj.getString("id"));
                kanjiaActivitySearchParams.setMemberId(UserContext.getCurrentUser().getId());
                kanjiaActivitySearchParams.setStatus(KanJiaStatusEnum.SUCCESS.name());
                KanjiaActivity kanjiaActivity = kanjiaActivityService.getKanjiaActivity(kanjiaActivitySearchParams);

                if (kanjiaActivity == null) {
                    throw new ServiceException(ResultCode.KANJIA_ACTIVITY_NOT_FOUND_ERROR);
                } else if (!KanJiaStatusEnum.SUCCESS.name().equals(kanjiaActivity.getStatus())) {
                    cartSkuVO.setKanjiaId(kanjiaActivity.getId());
                    cartSkuVO.setPurchasePrice(0D);
                    throw new ServiceException(ResultCode.KANJIA_ACTIVITY_NOT_PASS_ERROR);
                }
                cartSkuVO.setKanjiaId(kanjiaActivity.getId());
                cartSkuVO.setNum(1);
            }
        }
    }

    /**
     * 校验会员积分余额与积分商品活动库存，并设置积分商品价格及积分消耗值。
     *
     * @param cartSkuVO 购物车SKU信息
     */
    private void checkPoint(CartSkuVO cartSkuVO) {
        PointsGoodsVO pointsGoodsVO = pointsGoodsService.getPointsGoodsDetailBySkuId(cartSkuVO.getGoodsSku().getId());
        if (pointsGoodsVO != null) {
            Member userInfo = memberService.getUserInfo();
            if (userInfo.getPoint() < pointsGoodsVO.getPoints()) {
                throw new ServiceException(ResultCode.POINT_NOT_ENOUGH);
            }
            if (pointsGoodsVO.getActiveStock() < 1) {
                throw new ServiceException(ResultCode.POINT_GOODS_ACTIVE_STOCK_INSUFFICIENT);
            }
            cartSkuVO.setPoint(pointsGoodsVO.getPoints());
            cartSkuVO.setPurchasePrice(0D);
            cartSkuVO.setPointsId(pointsGoodsVO.getId());
        }
    }
}
