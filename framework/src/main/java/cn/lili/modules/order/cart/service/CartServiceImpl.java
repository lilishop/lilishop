package cn.lili.modules.order.cart.service;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberAddress;
import cn.lili.modules.member.service.MemberAddressService;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.cart.entity.dto.MemberCouponDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.entity.vo.TradeParams;
import cn.lili.modules.order.cart.render.TradeBuilder;
import cn.lili.modules.order.order.entity.dos.Trade;
import cn.lili.modules.order.order.entity.vo.ReceiptVO;
import cn.lili.modules.promotion.entity.dos.KanjiaActivity;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.search.KanjiaActivitySearchParams;
import cn.lili.modules.promotion.entity.dto.search.MemberCouponSearchParams;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.enums.KanJiaStatusEnum;
import cn.lili.modules.promotion.entity.enums.MemberCouponStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.vos.PointsGoodsVO;
import cn.lili.modules.promotion.service.KanjiaActivityService;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.modules.promotion.service.PointsGoodsService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.modules.search.service.EsGoodsSearchService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 购物车业务层实现
 *
 * @author Chopper
 * @since 2020-03-23 12:29 下午
 */
@Slf4j
@Service
public class CartServiceImpl implements CartService {

    static String errorMessage = "购物车异常，请稍后重试";

    /**
     * 缓存
     */
    @Autowired
    private Cache<Object> cache;
    /**
     * 会员优惠券
     */
    @Autowired
    private MemberCouponService memberCouponService;
    /**
     * 规格商品
     */
    @Autowired
    private GoodsSkuService goodsSkuService;
    /**
     * 促销商品
     */
    @Autowired
    private PointsGoodsService pointsGoodsService;
    /**
     * 会员地址
     */
    @Autowired
    private MemberAddressService memberAddressService;
    /**
     * ES商品
     */
    @Autowired
    private EsGoodsSearchService esGoodsSearchService;
    /**
     * 商品索引
     */
    @Autowired
    private EsGoodsIndexService goodsIndexService;
    /**
     * ES商品
     */
    @Autowired
    private GoodsService goodsService;
    /**
     * 砍价
     */
    @Autowired
    private KanjiaActivityService kanjiaActivityService;
    /**
     * 交易
     */
    @Autowired
    private TradeBuilder tradeBuilder;

    @Autowired
    private MemberService memberService;

    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Override
    public void add(String skuId, Integer num, String cartType, Boolean cover) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        if (num <= 0) {
            throw new ServiceException(ResultCode.CART_NUM_ERROR);
        }
        CartTypeEnum cartTypeEnum = getCartType(cartType);
        GoodsSku dataSku = checkGoods(skuId);
        Map<String, Object> promotionMap = this.getCurrentGoodsPromotion(dataSku, cartType);

        try {
            //购物车方式购买需要保存之前的选择，其他方式购买，则直接抹除掉之前的记录
            TradeDTO tradeDTO;
            if (cartTypeEnum.equals(CartTypeEnum.CART)) {

                //如果存在，则变更数量不做新增，否则新增一个商品进入集合
                tradeDTO = this.readDTO(cartTypeEnum);
                List<CartSkuVO> cartSkuVOS = tradeDTO.getSkuList();
                CartSkuVO cartSkuVO = cartSkuVOS.stream().filter(i -> i.getGoodsSku().getId().equals(skuId)).findFirst().orElse(null);


                //购物车中已经存在，更新数量
                if (cartSkuVO != null && dataSku.getUpdateTime().equals(cartSkuVO.getGoodsSku().getUpdateTime())) {

                    //如果覆盖购物车中商品数量
                    if (Boolean.TRUE.equals(cover)) {
                        cartSkuVO.setNum(num);
                        this.checkSetGoodsQuantity(cartSkuVO, skuId, num);
                    } else {
                        int oldNum = cartSkuVO.getNum();
                        int newNum = oldNum + num;
                        this.checkSetGoodsQuantity(cartSkuVO, skuId, newNum);
                    }

                    //计算购物车小计
                    cartSkuVO.setSubTotal(CurrencyUtil.mul(cartSkuVO.getPurchasePrice(), cartSkuVO.getNum()));
                } else {

                    //先清理一下 如果商品无效的话
                    cartSkuVOS.remove(cartSkuVO);
                    //购物车中不存在此商品，则新建立一个
                    cartSkuVO = new CartSkuVO(dataSku, promotionMap);

                    cartSkuVO.setCartType(cartTypeEnum);
                    //再设置加入购物车的数量
                    this.checkSetGoodsQuantity(cartSkuVO, skuId, num);
                    //计算购物车小计
                    cartSkuVO.setSubTotal(CurrencyUtil.mul(cartSkuVO.getPurchasePrice(), cartSkuVO.getNum()));
                    cartSkuVOS.add(cartSkuVO);
                }

                //新加入的商品都是选中的
                cartSkuVO.setChecked(true);
            } else {
                tradeDTO = new TradeDTO(cartTypeEnum);
                tradeDTO.setMemberId(currentUser.getId());
                tradeDTO.setMemberName(currentUser.getUsername());
                List<CartSkuVO> cartSkuVOS = tradeDTO.getSkuList();

                //购物车中不存在此商品，则新建立一个
                CartSkuVO cartSkuVO = new CartSkuVO(dataSku, promotionMap);
                cartSkuVO.setCartType(cartTypeEnum);
                //检测购物车数据
                checkCart(cartTypeEnum, cartSkuVO, skuId, num);
                //计算购物车小计
                cartSkuVO.setSubTotal(CurrencyUtil.mul(cartSkuVO.getPurchasePrice(), cartSkuVO.getNum()));
                cartSkuVOS.add(cartSkuVO);
            }

            tradeDTO.setCartTypeEnum(cartTypeEnum);
            //如购物车发生更改，则重置优惠券
            tradeDTO.setStoreCoupons(null);
            tradeDTO.setPlatformCoupon(null);
            this.resetTradeDTO(tradeDTO);
        } catch (ServiceException serviceException) {
            throw serviceException;
        } catch (Exception e) {
            log.error("购物车渲染异常", e);
            throw new ServiceException(errorMessage);
        }
    }

    /**
     * 读取当前会员购物原始数据key
     *
     * @param cartTypeEnum 获取方式
     * @return 当前会员购物原始数据key
     */
    private String getOriginKey(CartTypeEnum cartTypeEnum) {

        //缓存key，默认使用购物车
        if (cartTypeEnum != null) {
            AuthUser currentUser = UserContext.getCurrentUser();
            return cartTypeEnum.getPrefix() + currentUser.getId();
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @Override
    public TradeDTO readDTO(CartTypeEnum checkedWay) {
        TradeDTO tradeDTO = (TradeDTO) cache.get(this.getOriginKey(checkedWay));
        if (tradeDTO == null) {
            tradeDTO = new TradeDTO(checkedWay);
            AuthUser currentUser = UserContext.getCurrentUser();
            tradeDTO.setMemberId(currentUser.getId());
            tradeDTO.setMemberName(currentUser.getUsername());
        }
        if (tradeDTO.getMemberAddress() == null) {
            tradeDTO.setMemberAddress(this.memberAddressService.getDefaultMemberAddress());
        }
        return tradeDTO;
    }

    @Override
    public void checked(String skuId, boolean checked) {
        TradeDTO tradeDTO = this.readDTO(CartTypeEnum.CART);
        List<CartSkuVO> cartSkuVOS = tradeDTO.getSkuList();
        for (CartSkuVO cartSkuVO : cartSkuVOS) {
            if (cartSkuVO.getGoodsSku().getId().equals(skuId)) {
                cartSkuVO.setChecked(checked);
            }
        }
        cache.put(this.getOriginKey(CartTypeEnum.CART), tradeDTO);
    }

    @Override
    public void checkedStore(String storeId, boolean checked) {
        TradeDTO tradeDTO = this.readDTO(CartTypeEnum.CART);
        List<CartSkuVO> cartSkuVOS = tradeDTO.getSkuList();
        for (CartSkuVO cartSkuVO : cartSkuVOS) {
            if (cartSkuVO.getStoreId().equals(storeId)) {
                cartSkuVO.setChecked(checked);
            }
        }
        cache.put(this.getOriginKey(CartTypeEnum.CART), tradeDTO);
    }

    @Override
    public void checkedAll(boolean checked) {
        TradeDTO tradeDTO = this.readDTO(CartTypeEnum.CART);
        List<CartSkuVO> cartSkuVOS = tradeDTO.getSkuList();
        for (CartSkuVO cartSkuVO : cartSkuVOS) {
            cartSkuVO.setChecked(checked);
        }
        cache.put(this.getOriginKey(CartTypeEnum.CART), tradeDTO);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(String[] skuIds) {
        TradeDTO tradeDTO = this.readDTO(CartTypeEnum.CART);
        List<CartSkuVO> cartSkuVOS = tradeDTO.getSkuList();
        List<CartSkuVO> deleteVos = new ArrayList<>();
        for (CartSkuVO cartSkuVO : cartSkuVOS) {
            for (String skuId : skuIds) {
                if (cartSkuVO.getGoodsSku().getId().equals(skuId)) {
                    deleteVos.add(cartSkuVO);
                }
            }
        }
        cartSkuVOS.removeAll(deleteVos);
        cache.put(this.getOriginKey(CartTypeEnum.CART), tradeDTO);
    }

    @Override
    public void clean() {
        cache.remove(this.getOriginKey(CartTypeEnum.CART));
    }

    public void cleanChecked(TradeDTO tradeDTO) {
        List<CartSkuVO> cartSkuVOS = tradeDTO.getSkuList();
        List<CartSkuVO> deleteVos = new ArrayList<>();
        for (CartSkuVO cartSkuVO : cartSkuVOS) {
            if (Boolean.TRUE.equals(cartSkuVO.getChecked())) {
                deleteVos.add(cartSkuVO);
            }
        }
        cartSkuVOS.removeAll(deleteVos);
        //清除选择的优惠券
        tradeDTO.setPlatformCoupon(null);
        tradeDTO.setStoreCoupons(null);
        //清除添加过的备注
        tradeDTO.setStoreRemark(null);
        cache.put(this.getOriginKey(tradeDTO.getCartTypeEnum()), tradeDTO);
    }

    @Override
    public void cleanChecked(CartTypeEnum way) {
        if (way.equals(CartTypeEnum.CART)) {
            TradeDTO tradeDTO = this.readDTO(CartTypeEnum.CART);
            this.cleanChecked(tradeDTO);
        } else {
            cache.remove(this.getOriginKey(way));
        }
    }

    @Override
    public void resetTradeDTO(TradeDTO tradeDTO) {
        cache.put(this.getOriginKey(tradeDTO.getCartTypeEnum()), tradeDTO);
    }

    @Override
    public TradeDTO getCheckedTradeDTO(CartTypeEnum way) {
        return tradeBuilder.buildChecked(way);
    }

    /**
     * 获取可使用的优惠券数量
     *
     * @param checkedWay 购物车购买：CART/立即购买：BUY_NOW/拼团购买：PINTUAN / 积分购买：POINT
     * @return 可使用的优惠券数量
     */
    @Override
    public Long getCanUseCoupon(CartTypeEnum checkedWay) {
        TradeDTO tradeDTO = this.readDTO(checkedWay);
        long count = 0L;
        double totalPrice = tradeDTO.getSkuList().stream().mapToDouble(i -> i.getPurchasePrice() * i.getNum()).sum();
        if (tradeDTO.getSkuList() != null && !tradeDTO.getSkuList().isEmpty()) {
            List<String> ids = tradeDTO.getSkuList().stream().filter(i -> Boolean.TRUE.equals(i.getChecked())).map(i -> i.getGoodsSku().getId()).collect(Collectors.toList());

            List<EsGoodsIndex> esGoodsList = esGoodsSearchService.getEsGoodsBySkuIds(ids);
            for (EsGoodsIndex esGoodsIndex : esGoodsList) {
                if (esGoodsIndex != null && esGoodsIndex.getPromotionMap() != null && !esGoodsIndex.getPromotionMap().isEmpty()) {
                    List<String> couponIds = esGoodsIndex.getPromotionMap().keySet().stream().filter(i -> i.contains(PromotionTypeEnum.COUPON.name())).map(i -> i.substring(i.lastIndexOf("-") + 1)).collect(Collectors.toList());
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

            //获取可操作的优惠券集合
            List<MemberCoupon> allScopeMemberCoupon = memberCouponService.getAllScopeMemberCoupon(tradeDTO.getMemberId(), storeIds);
            if (allScopeMemberCoupon != null && !allScopeMemberCoupon.isEmpty()) {
                //过滤满足消费门槛
                count += allScopeMemberCoupon.stream().filter(i -> i.getConsumeThreshold() <= totalPrice).count();
            }
        }
        return count;
    }

    @Override
    public TradeDTO getAllTradeDTO() {
        return tradeBuilder.buildCart(CartTypeEnum.CART);
    }

    /**
     * 校验商品有效性，判定失效和库存，促销活动价格
     *
     * @param skuId 商品skuId
     */
    private GoodsSku checkGoods(String skuId) {
        GoodsSku dataSku = this.goodsSkuService.getGoodsSkuByIdFromCache(skuId);
        if (dataSku == null) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        if (!GoodsAuthEnum.PASS.name().equals(dataSku.getAuthFlag()) || !GoodsStatusEnum.UPPER.name().equals(dataSku.getMarketEnable())) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        return dataSku;
    }

    /**
     * 检查并设置购物车商品数量
     *
     * @param cartSkuVO 购物车商品对象
     * @param skuId     商品id
     * @param num       购买数量
     */
    private void checkSetGoodsQuantity(CartSkuVO cartSkuVO, String skuId, Integer num) {
        Integer enableStock = goodsSkuService.getStock(skuId);

        //如果sku的可用库存小于等于0或者小于用户购买的数量，则不允许购买
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

    @Override
    public void shippingAddress(String shippingAddressId, String way) {

        //默认购物车
        CartTypeEnum cartTypeEnum = CartTypeEnum.CART;
        if (CharSequenceUtil.isNotEmpty(way)) {
            cartTypeEnum = CartTypeEnum.valueOf(way);
        }

        TradeDTO tradeDTO = this.readDTO(cartTypeEnum);
        MemberAddress memberAddress = memberAddressService.getById(shippingAddressId);
        tradeDTO.setMemberAddress(memberAddress);
        this.resetTradeDTO(tradeDTO);
    }

    /**
     * 选择发票
     *
     * @param receiptVO 发票信息
     * @param way       购物车类型
     */
    @Override
    public void shippingReceipt(ReceiptVO receiptVO, String way) {
        CartTypeEnum cartTypeEnum = CartTypeEnum.CART;
        if (CharSequenceUtil.isNotEmpty(way)) {
            cartTypeEnum = CartTypeEnum.valueOf(way);
        }
        TradeDTO tradeDTO = this.readDTO(cartTypeEnum);
        tradeDTO.setNeedReceipt(true);
        tradeDTO.setReceiptVO(receiptVO);
        this.resetTradeDTO(tradeDTO);
    }

    /**
     * 选择配送方式
     *
     * @param storeId        店铺id
     * @param deliveryMethod 配送方式
     * @param way            购物车类型
     */
    @Override
    public void shippingMethod(String storeId, String deliveryMethod, String way) {
        CartTypeEnum cartTypeEnum = CartTypeEnum.CART;
        if (CharSequenceUtil.isNotEmpty(way)) {
            cartTypeEnum = CartTypeEnum.valueOf(way);
        }
        TradeDTO tradeDTO = this.readDTO(cartTypeEnum);
        for (CartVO cartVO : tradeDTO.getCartList()) {
            if (cartVO.getStoreId().equals(storeId)) {
                cartVO.setDeliveryMethod(DeliveryMethodEnum.valueOf(deliveryMethod).name());
            }
        }
        this.resetTradeDTO(tradeDTO);
    }

    /**
     * 获取购物车商品数量
     *
     * @param checked 是否选择
     * @return 购物车商品数量
     */
    @Override
    public Long getCartNum(Boolean checked) {
        //构建购物车
        TradeDTO tradeDTO = this.getAllTradeDTO();
        //过滤sku列表
        List<CartSkuVO> collect = tradeDTO.getSkuList().stream().filter(i -> Boolean.FALSE.equals(i.getInvalid())).collect(Collectors.toList());
        long count = 0L;
        if (!tradeDTO.getSkuList().isEmpty()) {
            if (checked != null) {
                count = collect.stream().filter(i -> i.getChecked().equals(checked)).count();
            } else {
                count = collect.size();
            }
        }
        return count;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void selectCoupon(String couponId, String way, boolean use) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        //获取购物车，然后重新写入优惠券
        CartTypeEnum cartTypeEnum = getCartType(way);
        TradeDTO tradeDTO = this.readDTO(cartTypeEnum);

        MemberCouponSearchParams searchParams = new MemberCouponSearchParams();
        searchParams.setMemberCouponStatus(MemberCouponStatusEnum.NEW.name());
        searchParams.setMemberId(currentUser.getId());
        searchParams.setId(couponId);
        MemberCoupon memberCoupon = memberCouponService.getMemberCoupon(searchParams);
        if (memberCoupon == null) {
            throw new ServiceException(ResultCode.COUPON_EXPIRED);
        }
        //使用优惠券 与否
        if (use) {
            this.useCoupon(tradeDTO, memberCoupon, cartTypeEnum);
        } else {
            if (Boolean.TRUE.equals(memberCoupon.getPlatformFlag())) {
                tradeDTO.setPlatformCoupon(null);
            } else {
                tradeDTO.getStoreCoupons().remove(memberCoupon.getStoreId());
            }
        }
        this.resetTradeDTO(tradeDTO);
    }


    @Override
    public Trade createTrade(TradeParams tradeParams) {
        //获取购物车
        CartTypeEnum cartTypeEnum = getCartType(tradeParams.getWay());
        TradeDTO tradeDTO = this.readDTO(cartTypeEnum);
        //设置基础属性
        tradeDTO.setClientType(tradeParams.getClient());
        tradeDTO.setStoreRemark(tradeParams.getRemark());
        tradeDTO.setParentOrderSn(tradeParams.getParentOrderSn());
        //订单无收货地址校验
        if (tradeDTO.getMemberAddress() == null) {
            throw new ServiceException(ResultCode.MEMBER_ADDRESS_NOT_EXIST);
        }
        //构建交易
        Trade trade = tradeBuilder.createTrade(tradeDTO);
        this.cleanChecked(this.readDTO(cartTypeEnum));
        return trade;
    }

    private Map<String, Object> getCurrentGoodsPromotion(GoodsSku dataSku, String cartType) {
        Map<String, Object> promotionMap;
        EsGoodsIndex goodsIndex = goodsIndexService.findById(dataSku.getId());
        if (goodsIndex == null) {
            GoodsVO goodsVO = this.goodsService.getGoodsVO(dataSku.getGoodsId());
            goodsIndex = goodsIndexService.getResetEsGoodsIndex(dataSku, goodsVO.getGoodsParamsDTOList());
        }
        if (goodsIndex.getPromotionMap() != null && !goodsIndex.getPromotionMap().isEmpty()) {
            if (goodsIndex.getPromotionMap().keySet().stream().anyMatch(i -> i.contains(PromotionTypeEnum.SECKILL.name())) ||
                    (goodsIndex.getPromotionMap().keySet().stream().anyMatch(i -> i.contains(PromotionTypeEnum.PINTUAN.name()))
                            && CartTypeEnum.PINTUAN.name().equals(cartType))) {

                Optional<Map.Entry<String, Object>> containsPromotion = goodsIndex.getPromotionMap().entrySet().stream().filter(i ->
                        i.getKey().contains(PromotionTypeEnum.SECKILL.name()) || i.getKey().contains(PromotionTypeEnum.PINTUAN.name())).findFirst();
                if (containsPromotion.isPresent()) {
                    JSONObject promotionsObj = JSONUtil.parseObj(containsPromotion.get().getValue());
                    PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
                    searchParams.setSkuId(dataSku.getId());
                    searchParams.setPromotionId(promotionsObj.get("id").toString());
                    PromotionGoods promotionsGoods = promotionGoodsService.getPromotionsGoods(searchParams);
                    if (promotionsGoods != null && promotionsGoods.getPrice() != null) {
                        dataSku.setPromotionFlag(true);
                        dataSku.setPromotionPrice(promotionsGoods.getPrice());
                    } else {
                        dataSku.setPromotionFlag(false);
                        dataSku.setPromotionPrice(null);
                    }
                }
            }
            promotionMap = goodsIndex.getPromotionMap();
        } else {
            promotionMap = null;
        }
        return promotionMap;
    }


    /**
     * 获取购物车类型
     *
     * @param way
     * @return
     */
    private CartTypeEnum getCartType(String way) {
        //默认购物车
        CartTypeEnum cartTypeEnum = CartTypeEnum.CART;
        if (CharSequenceUtil.isNotEmpty(way)) {
            try {
                cartTypeEnum = CartTypeEnum.valueOf(way);
            } catch (IllegalArgumentException e) {
                log.error("获取购物车类型出现错误：", e);
            }
        }
        return cartTypeEnum;
    }

    /**
     * 使用优惠券判定
     *
     * @param tradeDTO     交易对象
     * @param memberCoupon 会员优惠券
     * @param cartTypeEnum 购物车
     */
    private void useCoupon(TradeDTO tradeDTO, MemberCoupon memberCoupon, CartTypeEnum cartTypeEnum) {

        //截取符合优惠券的商品
        List<CartSkuVO> cartSkuVOS = checkCoupon(memberCoupon, tradeDTO);

        //定义使用优惠券的信息商品信息
        Map<String, Double> skuPrice = new HashMap<>(1);


        //购物车价格
        double cartPrice = 0d;

        //循环符合优惠券的商品
        for (CartSkuVO cartSkuVO : cartSkuVOS) {
            if (Boolean.FALSE.equals(cartSkuVO.getChecked())) {
                continue;
            }
            //有促销金额则用促销金额，否则用商品原价
            if (cartSkuVO.getPromotionMap() != null && !cartSkuVO.getPromotionMap().isEmpty()) {
                if (cartSkuVO.getPromotionMap().keySet().stream().anyMatch(i -> i.contains(PromotionTypeEnum.PINTUAN.name()) || i.contains(PromotionTypeEnum.SECKILL.name()))) {
                    cartPrice = CurrencyUtil.add(cartPrice, CurrencyUtil.mul(cartSkuVO.getPurchasePrice(), cartSkuVO.getNum()));
                    skuPrice.put(cartSkuVO.getGoodsSku().getId(), CurrencyUtil.mul(cartSkuVO.getPurchasePrice(), cartSkuVO.getNum()));
                } else {
                    cartPrice = CurrencyUtil.add(cartPrice, CurrencyUtil.mul(cartSkuVO.getGoodsSku().getPrice(), cartSkuVO.getNum()));
                    skuPrice.put(cartSkuVO.getGoodsSku().getId(), CurrencyUtil.mul(cartSkuVO.getGoodsSku().getPrice(), cartSkuVO.getNum()));
                }
            }
        }


        //如果购物车金额大于消费门槛则使用
        if (cartPrice >= memberCoupon.getConsumeThreshold()) {
            //如果是平台优惠券
            if (Boolean.TRUE.equals(memberCoupon.getPlatformFlag())) {
                tradeDTO.setPlatformCoupon(new MemberCouponDTO(skuPrice, memberCoupon));
            } else {
                tradeDTO.getStoreCoupons().put(memberCoupon.getStoreId(), new MemberCouponDTO(skuPrice, memberCoupon));
            }
        }

    }

    /**
     * 获取可以使用优惠券的商品信息
     *
     * @param memberCoupon 用于计算优惠券结算详情
     * @param tradeDTO     购物车信息
     * @return 是否可以使用优惠券
     */
    private List<CartSkuVO> checkCoupon(MemberCoupon memberCoupon, TradeDTO tradeDTO) {
        List<CartSkuVO> cartSkuVOS;
        //如果是店铺优惠券，判定的内容
        if (Boolean.FALSE.equals(memberCoupon.getPlatformFlag())) {
            cartSkuVOS = tradeDTO.getSkuList().stream().filter(i -> i.getStoreId().equals(memberCoupon.getStoreId())).collect(Collectors.toList());
        }
        //否则为平台优惠券，筛选商品为全部商品
        else {
            cartSkuVOS = tradeDTO.getSkuList();
        }

        //当初购物车商品中是否存在符合优惠券条件的商品sku
        if (memberCoupon.getScopeType().equals(PromotionsScopeTypeEnum.ALL.name())) {
            return cartSkuVOS;
        } else if (memberCoupon.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_GOODS_CATEGORY.name())) {
            //分类路径是否包含
            return cartSkuVOS.stream().filter(i -> CharSequenceUtil.contains(memberCoupon.getScopeId(), i.getGoodsSku().getCategoryPath())).collect(Collectors.toList());
        } else if (memberCoupon.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_GOODS.name())) {
            //范围关联ID是否包含
            return cartSkuVOS.stream().filter(i -> CharSequenceUtil.contains(memberCoupon.getScopeId(), i.getGoodsSku().getId())).collect(Collectors.toList());
        } else if (memberCoupon.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_SHOP_CATEGORY.name())) {
            //店铺分类路径是否包含
            return cartSkuVOS.stream().filter(i -> CharSequenceUtil.contains(memberCoupon.getScopeId(), i.getGoodsSku().getStoreCategoryPath())).collect(Collectors.toList());
        }
        return new ArrayList<>();
    }

    /**
     * 检测购物车
     *
     * @param cartTypeEnum 购物车枚举
     * @param cartSkuVO    SKUVO
     * @param skuId        SkuId
     * @param num          数量
     */
    private void checkCart(CartTypeEnum cartTypeEnum, CartSkuVO cartSkuVO, String skuId, Integer num) {

        this.checkSetGoodsQuantity(cartSkuVO, skuId, num);
        //拼团判定
        if (cartTypeEnum.equals(CartTypeEnum.PINTUAN)) {
            //砍价判定
            checkPintuan(cartSkuVO);
        } else if (cartTypeEnum.equals(CartTypeEnum.KANJIA)) {
            //检测购物车的数量
            checkKanjia(cartSkuVO);
        } else if (cartTypeEnum.equals(CartTypeEnum.POINTS)) {
            //检测购物车的数量
            checkPoint(cartSkuVO);
        }
    }

    /**
     * 校验拼团信息
     *
     * @param cartSkuVO 购物车信息
     */
    private void checkPintuan(CartSkuVO cartSkuVO) {
        //拼团活动，需要对限购数量进行判定
        //获取拼团信息
        if (cartSkuVO.getPromotionMap() != null && !cartSkuVO.getPromotionMap().isEmpty()) {
            Optional<Map.Entry<String, Object>> pintuanPromotions = cartSkuVO.getPromotionMap().entrySet().stream().filter(i -> i.getKey().contains(PromotionTypeEnum.PINTUAN.name())).findFirst();
            if (pintuanPromotions.isPresent()) {
                JSONObject promotionsObj = JSONUtil.parseObj(pintuanPromotions.get().getValue());
                //写入拼团信息
                cartSkuVO.setPintuanId(promotionsObj.get("id").toString());
                //检测拼团限购数量
                Integer limitNum = promotionsObj.get("limitNum", Integer.class);
                if (limitNum != 0 && cartSkuVO.getNum() > limitNum) {
                    throw new ServiceException(ResultCode.CART_PINTUAN_LIMIT_ERROR);
                }
            }
        }
    }

    /**
     * 校验砍价信息
     *
     * @param cartSkuVO 购物车信息
     */
    private void checkKanjia(CartSkuVO cartSkuVO) {
        if (cartSkuVO.getPromotionMap() != null && !cartSkuVO.getPromotionMap().isEmpty()) {
            Optional<Map.Entry<String, Object>> kanjiaPromotions = cartSkuVO.getPromotionMap().entrySet().stream().filter(i -> i.getKey().contains(PromotionTypeEnum.KANJIA.name())).findFirst();
            if (kanjiaPromotions.isPresent()) {
                JSONObject promotionsObj = JSONUtil.parseObj(kanjiaPromotions.get().getValue());
                //查找当前会员的砍价商品活动
                KanjiaActivitySearchParams kanjiaActivitySearchParams = new KanjiaActivitySearchParams();
                kanjiaActivitySearchParams.setKanjiaActivityGoodsId(promotionsObj.get("id", String.class));
                kanjiaActivitySearchParams.setMemberId(UserContext.getCurrentUser().getId());
                kanjiaActivitySearchParams.setStatus(KanJiaStatusEnum.SUCCESS.name());
                KanjiaActivity kanjiaActivity = kanjiaActivityService.getKanjiaActivity(kanjiaActivitySearchParams);

                //校验砍价活动是否满足条件
                //判断发起砍价活动
                if (kanjiaActivity == null) {
                    throw new ServiceException(ResultCode.KANJIA_ACTIVITY_NOT_FOUND_ERROR);
                    //判断砍价活动是否已满足条件
                } else if (!KanJiaStatusEnum.SUCCESS.name().equals(kanjiaActivity.getStatus())) {
                    cartSkuVO.setKanjiaId(kanjiaActivity.getId());
                    cartSkuVO.setPurchasePrice(0D);
                    throw new ServiceException(ResultCode.KANJIA_ACTIVITY_NOT_PASS_ERROR);
                }
                //砍价商品默认一件货物
                cartSkuVO.setKanjiaId(kanjiaActivity.getId());
                cartSkuVO.setNum(1);
            }
        }
    }

    /**
     * 校验积分商品信息
     *
     * @param cartSkuVO 购物车信息
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
