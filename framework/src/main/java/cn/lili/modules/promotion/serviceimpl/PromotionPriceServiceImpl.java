package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dos.*;
import cn.lili.modules.promotion.entity.dto.*;
import cn.lili.modules.promotion.entity.enums.*;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.service.PromotionPriceService;
import cn.lili.modules.promotion.service.SeckillApplyService;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.service.EsGoodsSearchService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 促销价格计算业务层实现
 *
 * @author paulG
 * @date 2020/8/21
 **/
@Service
@Slf4j
public class PromotionPriceServiceImpl implements PromotionPriceService {

    /**
     * ES商品
     */
    @Autowired
    private EsGoodsSearchService goodsSearchService;
    /**
     * 秒杀活动申请
     */
    @Autowired
    private SeckillApplyService seckillApplyService;
    /**
     * 促销商品
     */
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    /**
     * 规格商品
     */
    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    public PromotionPriceDTO calculationPromotionPrice(List<PromotionPriceParamDTO> tradeSkuList, List<MemberCoupon> memberCouponList) {
        PromotionPriceDTO promotionPrice = new PromotionPriceDTO();
        if (!tradeSkuList.isEmpty()) {
            //拆分出SkuId列表
            List<String> skuIds = tradeSkuList.parallelStream().map(PromotionPriceParamDTO::getSkuId).collect(Collectors.toList());
            //参与计算的ES商品SKU及其促销信息列表
            List<EsGoodsIndex> esGoodsSkus = goodsSearchService.getEsGoodsBySkuIds(skuIds);
            //参与计算的缓存中的商品SKU列表
            List<GoodsSku> goodsSkus = goodsSkuService.getGoodsSkuByIdFromCache(skuIds);
            //计算价格
            promotionPrice = this.calculationPromotionPrice(tradeSkuList, memberCouponList, esGoodsSkus, goodsSkus);
        }
        return promotionPrice;
    }

    /**
     * 促销计算
     *
     * @param tradeSkuList     促销计算参数列表
     * @param memberCouponList 参与促销计算的优惠券列表
     * @param esGoodsSkus      参与计算的ES商品SKU及其促销信息列表
     * @param goodsSkus        参与计算的缓存中的商品SKU列表
     * @return 促销计算结果
     */
    private PromotionPriceDTO calculationPromotionPrice(List<PromotionPriceParamDTO> tradeSkuList, List<MemberCoupon> memberCouponList, List<EsGoodsIndex> esGoodsSkus, List<GoodsSku> goodsSkus) {
        PromotionPriceDTO promotionPrice = new PromotionPriceDTO();

        //单品商品SKU促销计算结果列表
        List<GoodsSkuPromotionPriceDTO> priceDTOList = this.packageGoodsSkuPromotionPrice(tradeSkuList, esGoodsSkus, goodsSkus);

        //将使用的优惠券根据店铺分类
        Map<String, List<MemberCoupon>> couponCollect = memberCouponList.parallelStream().collect(Collectors.groupingBy(MemberCoupon::getStoreId));

        double couponTotalPrice = 0;

        //根据卖家分组商品信息
        Map<String, List<GoodsSkuPromotionPriceDTO>> storeCollect = priceDTOList.parallelStream().collect(Collectors.groupingBy(GoodsSkuPromotionPriceDTO::getStoreId));
        List<StorePromotionPriceDTO> storePromotionPriceList = new ArrayList<>();
        for (Map.Entry<String, List<GoodsSkuPromotionPriceDTO>> entry : storeCollect.entrySet()) {
            StorePromotionPriceDTO storePromotionPrice = new StorePromotionPriceDTO();
            storePromotionPrice.setStoreId(entry.getKey());
            storePromotionPrice.setGoodsSkuPromotionPriceList(entry.getValue());
            Optional<GoodsSkuPromotionPriceDTO> firstFullDiscount = entry.getValue().parallelStream().filter(i -> i.getPromotionId() != null && i.getPromotionType().equals(PromotionTypeEnum.FULL_DISCOUNT.name())).findFirst();
            if (firstFullDiscount.isPresent()) {
                String promotionId = firstFullDiscount.get().getPromotionId();
                //是否存在满优惠促销活动
                String esPromotionFullDiscountKey = PromotionTypeEnum.FULL_DISCOUNT + "-" + promotionId;
                if (CharSequenceUtil.isNotEmpty(promotionId)) {
                    FullDiscount fullDiscount = null;
                    for (EsGoodsIndex skus : esGoodsSkus) {
                        //检查是否为正在进行的满优惠活动
                        if (skus.getPromotionMap() != null && skus.getPromotionMap().containsKey(esPromotionFullDiscountKey)) {
                            fullDiscount = (FullDiscount) skus.getPromotionMap().get(esPromotionFullDiscountKey);
                            break;
                        }
                    }
                    if (fullDiscount != null) {
                        //计算满优惠活动
                        this.calculationFullDiscount(fullDiscount, storePromotionPrice);
                    }
                }
            }

            //获取店铺优惠券
            List<MemberCoupon> storeCoupons = couponCollect.get(entry.getKey());
            if (storeCoupons != null && !storeCoupons.isEmpty()) {
                //计算店铺优惠券活动
                couponTotalPrice = this.calculationCoupon(storeCoupons, entry.getValue());
                storePromotionPrice.setTotalCouponPrice(couponTotalPrice);
                storePromotionPrice.setTotalFinalePrice(storePromotionPrice.getGoodsSkuPromotionPriceList().parallelStream().mapToDouble(GoodsSkuPromotionPriceDTO::getTotalFinalePrice).sum());
            }

            //累加除商品促销信息之外的信息
            this.accumulationGoodsSkuPromotionOther(entry.getValue(), storePromotionPrice);

            storePromotionPriceList.add(storePromotionPrice);
        }
        //获取平台优惠券
        List<MemberCoupon> platformCoupons = couponCollect.get("platform");
        if (platformCoupons != null && !platformCoupons.isEmpty()) {
            //计算平台优惠券活动
            couponTotalPrice = CurrencyUtil.add(couponTotalPrice, this.calculationCoupon(platformCoupons, priceDTOList));
            for (StorePromotionPriceDTO storePromotionPriceDTO : storePromotionPriceList) {
                Double couponPrice = storePromotionPriceDTO.getGoodsSkuPromotionPriceList().parallelStream().mapToDouble(GoodsSkuPromotionPriceDTO::getCouponPrice).sum();
                storePromotionPriceDTO.setTotalCouponPrice(couponPrice);
            }
        }
        promotionPrice.setStorePromotionPriceList(storePromotionPriceList);
        promotionPrice.setTotalCouponPrice(couponTotalPrice);
        promotionPrice.setTotalOriginPrice(storePromotionPriceList.parallelStream().mapToDouble(StorePromotionPriceDTO::getTotalOriginPrice).sum());
        promotionPrice.setTotalPoints(storePromotionPriceList.parallelStream().mapToDouble(StorePromotionPriceDTO::getTotalPoints).sum());
        promotionPrice.setTotalDiscountPrice(storePromotionPriceList.parallelStream().mapToDouble(StorePromotionPriceDTO::getTotalDiscountPrice).sum());
        //最终结算金额 = 商品原价格合计 - 总优惠价格合计 - 优惠券合计
        double totalFinalePrice = CurrencyUtil.sub(CurrencyUtil.sub(promotionPrice.getTotalOriginPrice(), promotionPrice.getTotalDiscountPrice()), promotionPrice.getTotalCouponPrice());
        promotionPrice.setTotalFinalePrice(totalFinalePrice);
        return promotionPrice;
    }


    /**
     * 单品SKU的促销计算
     *
     * @param tradeSkuList 交易商品信息列表
     * @param esGoodsSkus  参与计算的ES商品SKU及其促销信息列表
     * @param goodsSkus    参与计算的缓存中的商品SKU列表
     * @return 单品SKU促销计算结果列表
     */
    private List<GoodsSkuPromotionPriceDTO> packageGoodsSkuPromotionPrice(List<PromotionPriceParamDTO> tradeSkuList, List<EsGoodsIndex> esGoodsSkus, List<GoodsSku> goodsSkus) {
        List<GoodsSkuPromotionPriceDTO> priceDTOList = new ArrayList<>();
        for (GoodsSku skus : goodsSkus) {
            List<EsGoodsIndex> collect = esGoodsSkus.parallelStream().filter(i -> i != null && i.getId().equals(skus.getId())).collect(Collectors.toList());
            if (!collect.isEmpty()) {
                EsGoodsIndex esGoodsIndex = collect.get(0);
                //找出当前商品相应的结算参数
                PromotionPriceParamDTO tradeSku = tradeSkuList.parallelStream().filter(i -> i.getSkuId().equals(skus.getId())).findFirst().orElse(new PromotionPriceParamDTO());
                GoodsSkuPromotionPriceDTO goodsSkuPromotionPrice = new GoodsSkuPromotionPriceDTO(skus, tradeSku.getNum());
                //商品原价总价 = 商品原价 * 数量
                goodsSkuPromotionPrice.setTotalOriginalPrice(CurrencyUtil.mul(goodsSkuPromotionPrice.getOriginalPrice(), tradeSku.getNum()));
                //获取当前商品所有参加的促销活动
                Map<String, Object> promotionMap = esGoodsIndex.getPromotionMap();
                //是否计算拼团促销
                String pintuanId = tradeSku.getPintuanId() != null ? tradeSku.getPintuanId() : null;
                //如果商品促销列表存在促销活动
                this.calculationPromotionMap(promotionMap, goodsSkuPromotionPrice, esGoodsIndex, pintuanId);

                goodsSkuPromotionPrice.setTotalOriginalPrice(CurrencyUtil.mul(goodsSkuPromotionPrice.getOriginalPrice(), goodsSkuPromotionPrice.getNumber()));
                goodsSkuPromotionPrice.setTotalPoints(CurrencyUtil.mul(goodsSkuPromotionPrice.getPoints(), goodsSkuPromotionPrice.getNumber()));
                goodsSkuPromotionPrice.setTotalDiscountPrice(CurrencyUtil.mul(goodsSkuPromotionPrice.getDiscountPrice(), goodsSkuPromotionPrice.getNumber()));
                goodsSkuPromotionPrice.setTotalFinalePrice(CurrencyUtil.mul(goodsSkuPromotionPrice.getFinalePrice(), goodsSkuPromotionPrice.getNumber()));
                priceDTOList.add(goodsSkuPromotionPrice);
            }
        }
        return priceDTOList;
    }

    /**
     * 促销计算(秒杀活动，拼团)
     *
     * @param promotionMap           当前商品所有参加的促销活动
     * @param goodsSkuPromotionPrice 商品SKU促销计算信息
     * @param esGoodsIndex           ES商品信息
     * @param pintuanId              拼团id，标示是否计算拼团活动
     */
    private void calculationPromotionMap(Map<String, Object> promotionMap, GoodsSkuPromotionPriceDTO goodsSkuPromotionPrice, EsGoodsIndex esGoodsIndex, String pintuanId) {
        if (promotionMap != null && !promotionMap.isEmpty()) {
            //检查当前商品是否存在秒杀活动活动
            Optional<String> existSeckill = promotionMap.keySet().parallelStream().filter(i -> i.contains(PromotionTypeEnum.SECKILL.name())).findFirst();
            if (existSeckill.isPresent()) {
                Seckill seckill = (Seckill) promotionMap.get(existSeckill.get());
                //计算秒杀活动促销
                this.calculationSeckill(seckill, goodsSkuPromotionPrice);
                seckill.setPromotionName(PromotionTypeEnum.SECKILL.name());
                goodsSkuPromotionPrice.getJoinPromotion().add(seckill);
            }

            //检查当前商品是否存在拼团活动
            Optional<String> existPintuan = promotionMap.keySet().parallelStream().filter(i -> i.contains(PromotionTypeEnum.PINTUAN.name())).findFirst();
            if (existPintuan.isPresent() && pintuanId != null) {
                Pintuan pintuan = (Pintuan) promotionMap.get(existPintuan.get());
                //优惠的总价格 = 原商品总价 - 优惠后的商品总价
                double discountPrice = CurrencyUtil.sub(goodsSkuPromotionPrice.getOriginalPrice(), esGoodsIndex.getPromotionPrice());
                goodsSkuPromotionPrice.setDiscountPrice(discountPrice);
                goodsSkuPromotionPrice.setFinalePrice(esGoodsIndex.getPromotionPrice());
                pintuan.setPromotionName(PromotionTypeEnum.PINTUAN.name());
                goodsSkuPromotionPrice.getJoinPromotion().add(pintuan);
            }

            //检查当前商品是否存在满优惠活动
            Optional<String> existFullDiscount = promotionMap.keySet().parallelStream().filter(i -> i.contains(PromotionTypeEnum.FULL_DISCOUNT.name())).findFirst();
            if (existFullDiscount.isPresent()) {
                FullDiscount discount = (FullDiscount) promotionMap.get(existFullDiscount.get());
                goodsSkuPromotionPrice.setPromotionId(discount.getId());
                goodsSkuPromotionPrice.setPromotionType(PromotionTypeEnum.FULL_DISCOUNT.name());
            }


            Optional<String> existPointsGoods = promotionMap.keySet().parallelStream().filter(i -> i.contains(PromotionTypeEnum.POINTS_GOODS.name())).findFirst();
            if (existPointsGoods.isPresent()) {
                PointsGoods pointsGoods = (PointsGoods) promotionMap.get(existPointsGoods.get());
                goodsSkuPromotionPrice.setPoints(pointsGoods.getPoints().doubleValue());
            }
        }
    }

    /**
     * 优惠券计算
     *
     * @param coupons 使用的优惠券列表
     * @param list    商品促销价格信息列表
     * @return 优惠券总金额
     */
    private double calculationCoupon(List<MemberCoupon> coupons, List<GoodsSkuPromotionPriceDTO> list) {
        double couponTotalPrice = 0;
        for (MemberCoupon coupon : coupons) {
            if (CouponScopeTypeEnum.PORTION_GOODS.name().equals(coupon.getScopeType())) {
                String[] scopeSkuIds = coupon.getScopeId().split(",");
                List<GoodsSkuPromotionPriceDTO> conformCollect = list.parallelStream().filter(i -> Arrays.asList(scopeSkuIds).contains(i.getSkuId())).collect(Collectors.toList());
                //单个优惠券计算
                couponTotalPrice = this.calculationSingleCoupon(coupon, conformCollect);
            } else if (CouponScopeTypeEnum.ALL.name().equals(coupon.getScopeType())) {
                //单个优惠券计算
                couponTotalPrice = this.calculationSingleCoupon(coupon, list);
            } else if (CouponScopeTypeEnum.PORTION_GOODS_CATEGORY.name().equals(coupon.getScopeType()) || CouponScopeTypeEnum.PORTION_SHOP_CATEGORY.name().equals(coupon.getScopeType())) {
                List<GoodsSkuPromotionPriceDTO> collect = this.filterCoupon(coupon, list);
                //单个优惠券计算
                couponTotalPrice = this.calculationSingleCoupon(coupon, collect);
            }
        }
        return couponTotalPrice;
    }

    /**
     * 过滤优惠券范围内分类 商品sku促销价格信息列表
     *
     * @param coupon 会员优惠券信息
     * @param list   过滤的列表
     * @return 过滤后的商品价格信息列表
     */
    private List<GoodsSkuPromotionPriceDTO> filterCoupon(MemberCoupon coupon, List<GoodsSkuPromotionPriceDTO> list) {
        String[] scopeCategories = coupon.getScopeId().split(",");

        //过滤优惠券范围内分类 商品sku促销价格信息列表
        return list.parallelStream().filter(i -> {
            String[] split;
            if (CouponScopeTypeEnum.PORTION_GOODS_CATEGORY.name().equals(coupon.getScopeType())) {
                split = i.getCategoryPath().split("\\|");
            } else {
                split = i.getStoreCategoryPath().split("\\|");
            }
            for (String s : split) {
                if (Arrays.asList(scopeCategories).contains(s)) {
                    return true;
                }
            }
            return false;
        }).collect(Collectors.toList());
    }

    /**
     * 单个优惠券计算
     *
     * @param coupon         使用的优惠券信息
     * @param conformCollect 商品促销价格信息列表
     * @return 优惠券总金额
     */
    private double calculationSingleCoupon(MemberCoupon coupon, List<GoodsSkuPromotionPriceDTO> conformCollect) {
        double couponTotalPrice = 0;
        //合计优惠券范围内的所有商品的原价
        double totalPrice = conformCollect.parallelStream().mapToDouble(GoodsSkuPromotionPriceDTO::getTotalFinalePrice).sum();

        //根据优惠券优惠类型，判断是否满足条件
        if (CouponTypeEnum.PRICE.name().equals(coupon.getCouponType()) && coupon.getConsumeThreshold() <= totalPrice) {
            couponTotalPrice = CurrencyUtil.add(couponTotalPrice, coupon.getPrice());
        } else if (CouponTypeEnum.DISCOUNT.name().equals(coupon.getCouponType())) {
            double fullRate = coupon.getDiscount() >= 10 ? coupon.getDiscount() / 100 : coupon.getDiscount() / 10;
            double discountRatePrice = CurrencyUtil.sub(totalPrice, CurrencyUtil.mul(totalPrice, fullRate));

            couponTotalPrice = CurrencyUtil.add(couponTotalPrice, discountRatePrice);
        }

        //分配到每个商品的优惠券金额
        for (GoodsSkuPromotionPriceDTO goodsSkuPromotionPriceDTO : conformCollect) {
            double rate = CurrencyUtil.div(goodsSkuPromotionPriceDTO.getTotalFinalePrice(), totalPrice, 5);
            double distributeCouponPrice = CurrencyUtil.div(CurrencyUtil.mul(couponTotalPrice, rate), goodsSkuPromotionPriceDTO.getNumber());
            if (goodsSkuPromotionPriceDTO.getFinalePrice() != null) {
                goodsSkuPromotionPriceDTO.setFinalePrice(CurrencyUtil.sub(goodsSkuPromotionPriceDTO.getFinalePrice(), distributeCouponPrice));
            } else {
                goodsSkuPromotionPriceDTO.setFinalePrice(CurrencyUtil.sub(goodsSkuPromotionPriceDTO.getOriginalPrice(), distributeCouponPrice));
            }
            goodsSkuPromotionPriceDTO.setCouponPrice(CurrencyUtil.mul(distributeCouponPrice, goodsSkuPromotionPriceDTO.getNumber()));
            goodsSkuPromotionPriceDTO.setTotalFinalePrice(CurrencyUtil.mul(goodsSkuPromotionPriceDTO.getFinalePrice(), goodsSkuPromotionPriceDTO.getNumber()));
            BasePromotion basePromotion = new BasePromotion();
            basePromotion.setId(coupon.getId());
            if (coupon.getIsPlatform() != null && Boolean.TRUE.equals(coupon.getIsPlatform())) {
                basePromotion.setPromotionName("platformCoupon");
            } else {
                basePromotion.setPromotionName("storeCoupon");
            }
            basePromotion.setStartTime(coupon.getStartTime());
            basePromotion.setEndTime(coupon.getEndTime());
            goodsSkuPromotionPriceDTO.getJoinPromotion().add(basePromotion);
        }
        return couponTotalPrice;
    }

    /**
     * 计算秒杀活动
     *
     * @param seckill        秒杀活动信息
     * @param promotionPrice 商品SKU的计算促销信息
     */
    private void calculationSeckill(Seckill seckill, GoodsSkuPromotionPriceDTO promotionPrice) {
        //检查 活动有效时间 及 活动有效状态
        if (checkPromotionValidTime(seckill.getStartTime(), seckill.getEndTime(), PromotionTypeEnum.SECKILL.name(), seckill.getId()) && seckill.getPromotionStatus().equals(PromotionStatusEnum.START.name())) {
            LambdaQueryWrapper<SeckillApply> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.eq(SeckillApply::getSkuId, promotionPrice.getSkuId()).eq(SeckillApply::getSeckillId, seckill.getId());
            SeckillApply seckillApply = seckillApplyService.getOne(queryWrapper);
            if (seckillApply.getPromotionApplyStatus().equals(PromotionApplyStatusEnum.PASS.name())) {
                Integer quantity = promotionGoodsService.getPromotionGoodsStock(PromotionTypeEnum.SECKILL, seckill.getId(), seckillApply.getSkuId());
                int seckillTotalSaleNum = seckillApply.getSalesNum() + promotionPrice.getNumber();
                if (quantity >= seckillTotalSaleNum) {
                    //优惠的价格 = 原商品价 - 优惠后的商品价
                    double discountPrice = CurrencyUtil.sub(promotionPrice.getOriginalPrice(), seckillApply.getPrice());
                    promotionPrice.setDiscountPrice(discountPrice);
                    promotionPrice.setFinalePrice(seckillApply.getPrice());
                } else {
                    log.error("购买数量超出秒杀活动剩余数量");
                }
            }
        }
    }

    /**
     * 计算满优惠促销
     *
     * @param fullDiscount           满优惠信息
     * @param storePromotionPriceDTO 店铺促销计算信息
     */
    private void calculationFullDiscount(FullDiscount fullDiscount, StorePromotionPriceDTO storePromotionPriceDTO) {
        //检查 活动有效时间 及 活动有效状态
        if (checkPromotionValidTime(fullDiscount.getStartTime(), fullDiscount.getEndTime(), PromotionTypeEnum.FULL_DISCOUNT.name(), fullDiscount.getId()) && fullDiscount.getPromotionStatus().equals(PromotionStatusEnum.START.name())) {
            //是否免运费
            if (Boolean.TRUE.equals(fullDiscount.getIsFreeFreight())) {
                storePromotionPriceDTO.setIsFreeFreight(true);
            }
            List<GoodsSkuPromotionPriceDTO> goodsSkuPromotionPriceList = storePromotionPriceDTO.getGoodsSkuPromotionPriceList();

            //参与活动的商品总数量
            this.accumulationGoodsSkuPromotionPrice(goodsSkuPromotionPriceList, storePromotionPriceDTO);

            //参与优惠商品的成交金额是否满足满优惠条件
            if (fullDiscount.getFullMoney() <= storePromotionPriceDTO.getTotalJoinDiscountPrice()) {
                //判断是否为满减，反之则为满折扣
                if (Boolean.TRUE.equals(fullDiscount.getIsFullMinus())) {
                    //优惠总金额 = 原优惠总金额 + 满优惠减免的金额
                    storePromotionPriceDTO.setTotalDiscountPrice(CurrencyUtil.add(storePromotionPriceDTO.getTotalDiscountPrice(), fullDiscount.getFullMinus()));
                } else if (Boolean.TRUE.equals(fullDiscount.getIsFullRate())) {
                    double fullRate = fullDiscount.getFullRate() >= 10 ? fullDiscount.getFullRate() / 100 : fullDiscount.getFullRate() / 10;
                    //满优惠减免的金额 = 原参与优惠的总金额 * 满优惠折扣（百分比）
                    double discountPrice = CurrencyUtil.sub(storePromotionPriceDTO.getTotalJoinDiscountPrice(), CurrencyUtil.mul(storePromotionPriceDTO.getTotalJoinDiscountPrice(), fullRate));
                    //优惠总金额 = 原优惠总金额 + 满优惠减免的金额
                    storePromotionPriceDTO.setTotalDiscountPrice(CurrencyUtil.add(storePromotionPriceDTO.getTotalDiscountPrice(), discountPrice));
                }
            }

            //将计算完的促销价格，平均分配到参与促销的每个商品上去
            this.distributeStoreFullDiscountPromotionPrice(storePromotionPriceDTO, fullDiscount);
        }
    }


    /**
     * 计算累加促销活动商品价格
     *
     * @param goodsSkuPromotionPriceList 商品促销价格信息列表
     * @param storePromotionPriceDTO     店铺促销计算信息
     */
    private void accumulationGoodsSkuPromotionPrice(List<GoodsSkuPromotionPriceDTO> goodsSkuPromotionPriceList, StorePromotionPriceDTO storePromotionPriceDTO) {
        //数据累加
        double totalNotJoinDiscountPrice = 0;
        double totalJoinDiscountPrice = 0;
        double totalDiscountPrice = 0;

        for (GoodsSkuPromotionPriceDTO goodsSkuPromotionPrice : goodsSkuPromotionPriceList) {
            if (goodsSkuPromotionPrice.getPromotionId() != null && goodsSkuPromotionPrice.getPromotionType().equals(PromotionTypeEnum.FULL_DISCOUNT.name())) {
                totalJoinDiscountPrice = CurrencyUtil.add(totalJoinDiscountPrice, goodsSkuPromotionPrice.getTotalFinalePrice());
                totalDiscountPrice = CurrencyUtil.add(totalDiscountPrice, goodsSkuPromotionPrice.getDiscountPrice());
            } else {
                totalNotJoinDiscountPrice = CurrencyUtil.add(totalNotJoinDiscountPrice, goodsSkuPromotionPrice.getFinalePrice());
            }
        }
        storePromotionPriceDTO.setTotalDiscountPrice(totalDiscountPrice);
        storePromotionPriceDTO.setTotalJoinDiscountPrice(totalJoinDiscountPrice);
        storePromotionPriceDTO.setTotalNotJoinDiscountPrice(totalNotJoinDiscountPrice);

        for (GoodsSkuPromotionPriceDTO goodsSkuPromotionPriceDTO : goodsSkuPromotionPriceList) {
            double fullDiscountRate = CurrencyUtil.div(goodsSkuPromotionPriceDTO.getTotalFinalePrice(), totalJoinDiscountPrice);
            goodsSkuPromotionPriceDTO.setDiscountPriceRate(fullDiscountRate);
        }

    }

    /**
     * 计算累加促销活动商品价格其他信息
     *
     * @param goodsSkuPromotionPriceList 商品促销价格信息列表
     * @param storePromotionPriceDTO     店铺促销计算信息
     */
    private void accumulationGoodsSkuPromotionOther(List<GoodsSkuPromotionPriceDTO> goodsSkuPromotionPriceList, StorePromotionPriceDTO storePromotionPriceDTO) {
        double totalWeight = 0;
        double totalNum = 0D;
        double totalOriginPrice = 0;
        double totalDiscountPrice = 0;
        double totalPoints = 0;
        for (GoodsSkuPromotionPriceDTO goodsSkuPromotionPrice : goodsSkuPromotionPriceList) {
            totalWeight = CurrencyUtil.add(totalWeight, goodsSkuPromotionPrice.getWeight());
            totalNum = CurrencyUtil.add(totalNum, goodsSkuPromotionPrice.getNumber());
            totalPoints = CurrencyUtil.add(totalPoints, goodsSkuPromotionPrice.getTotalPoints());
            totalOriginPrice = CurrencyUtil.add(totalOriginPrice, (goodsSkuPromotionPrice.getOriginalPrice() * goodsSkuPromotionPrice.getNumber()));
            if (goodsSkuPromotionPrice.getTotalDiscountPrice() != null && goodsSkuPromotionPrice.getTotalDiscountPrice() > 0) {
                totalDiscountPrice = CurrencyUtil.add(totalDiscountPrice, goodsSkuPromotionPrice.getTotalDiscountPrice());
            }
        }
        if (storePromotionPriceDTO.getTotalDiscountPrice() == null && totalDiscountPrice > 0) {
            storePromotionPriceDTO.setTotalDiscountPrice(totalDiscountPrice);
        } else if (storePromotionPriceDTO.getTotalDiscountPrice() == null) {
            storePromotionPriceDTO.setTotalDiscountPrice(0d);
        }
        storePromotionPriceDTO.setTotalPoints(totalPoints);
        storePromotionPriceDTO.setTotalNum((int) totalNum);
        storePromotionPriceDTO.setTotalWeight(totalWeight);
        storePromotionPriceDTO.setTotalOriginPrice(totalOriginPrice);
    }


    /**
     * 将相应促销活动的店铺促销计算信息中的促销价格和最终成交金额平均分配到每个商品促销信息中去
     *
     * @param storePromotionPriceDTO 店铺促销计算信息
     */
    private void distributeStoreFullDiscountPromotionPrice(StorePromotionPriceDTO storePromotionPriceDTO, FullDiscount fullDiscount) {
        //分配的促销总金额 = 促销总金额 / 分配的数量
        for (GoodsSkuPromotionPriceDTO goodsSkuPromotionPrice : storePromotionPriceDTO.getGoodsSkuPromotionPriceList()) {
            //属于相应的促销活动的商品金额均分
            if (goodsSkuPromotionPrice.getDiscountPriceRate() != null
                    && goodsSkuPromotionPrice.getPromotionType() != null
                    && goodsSkuPromotionPrice.getPromotionType().equals(PromotionTypeEnum.FULL_DISCOUNT.name())) {
                double distributeDiscountTotalPrice = CurrencyUtil.mul(storePromotionPriceDTO.getTotalDiscountPrice(), goodsSkuPromotionPrice.getDiscountPriceRate());
                goodsSkuPromotionPrice.setDiscountPrice(distributeDiscountTotalPrice);
                goodsSkuPromotionPrice.setTotalDiscountPrice(distributeDiscountTotalPrice);
                //单品成交价
                double finalPrice = CurrencyUtil.sub(goodsSkuPromotionPrice.getTotalOriginalPrice(), distributeDiscountTotalPrice);
                goodsSkuPromotionPrice.setFinalePrice(finalPrice);
                goodsSkuPromotionPrice.setTotalFinalePrice(CurrencyUtil.mul(finalPrice, goodsSkuPromotionPrice.getNumber()));
                fullDiscount.setPromotionName(PromotionTypeEnum.FULL_DISCOUNT.name());
                goodsSkuPromotionPrice.getJoinPromotion().add(fullDiscount);

            }
        }
    }

    /**
     * 检查活动有效时间
     *
     * @param startTime     活动开始时间
     * @param endTime       活动结束时间
     * @param promotionType 活动类型
     * @param promotionId   活动ID
     * @return 是否有效
     */
    private boolean checkPromotionValidTime(Date startTime, Date endTime, String promotionType, String promotionId) {
        long now = System.currentTimeMillis();
        if (startTime.getTime() > now) {
            log.error("商品ID为{}的{}活动开始时间小于当时时间，活动未开始！", promotionId, promotionType);
            return false;
        }
        if (endTime.getTime() < now) {
            log.error("活动ID为{}的{}活动结束时间大于当时时间，活动已结束！", promotionId, promotionType);
            return false;
        }
        return true;
    }
}
