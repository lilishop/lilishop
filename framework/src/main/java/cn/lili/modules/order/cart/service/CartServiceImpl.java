package cn.lili.modules.order.cart.service;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.cache.Cache;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.MemberAddress;
import cn.lili.modules.order.cart.entity.dto.MemberCouponDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.cart.entity.enums.TradeCacheEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.entity.vo.TradeParams;
import cn.lili.modules.order.cart.render.TradeBuilder;
import cn.lili.modules.order.order.entity.dos.Trade;
import cn.lili.modules.order.order.entity.vo.ReceiptVO;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.enums.CouponScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.MemberCouponStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.service.MemberAddressService;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.service.EsGoodsSearchService;
import cn.lili.modules.store.entity.dos.FreightTemplateChild;
import cn.lili.modules.store.entity.enums.FreightTemplateEnum;
import cn.lili.modules.store.entity.vos.FreightTemplateVO;
import cn.lili.modules.store.service.FreightTemplateService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * 购物车业务层实现
 *
 * @author Chopper
 * @date 2020-03-23 12:29 下午
 */
@Slf4j
@Service
public class CartServiceImpl implements CartService {

    static String errorMessage = "购物车异常，请稍后重试";

    //缓存
    @Autowired
    private Cache<Object> cache;
    //会员优惠券
    @Autowired
    private MemberCouponService memberCouponService;
    //规格商品
    @Autowired
    private GoodsSkuService goodsSkuService;
    //促销商品
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    //会员地址
    @Autowired
    private MemberAddressService memberAddressService;
    //ES商品
    @Autowired
    private EsGoodsSearchService esGoodsSearchService;
    //运费模板
    @Autowired
    private FreightTemplateService freightTemplateService;
    //交易
    @Autowired
    private TradeBuilder tradeBuilder;

    @Override
    public void add(String skuId, Integer num, String cartType) {
        CartTypeEnum cartTypeEnum = getCartType(cartType);
        GoodsSku dataSku = checkGoods(skuId, num);
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
                    //判断是商品否被修改
                    int oldNum = cartSkuVO.getNum();
                    int newNum = oldNum + num;
                    this.checkSetGoodsQuantity(cartSkuVO, skuId, newNum);
                    //计算购物车小计
                    cartSkuVO.setSubTotal(CurrencyUtil.mul(cartSkuVO.getPurchasePrice(), cartSkuVO.getNum()));
                } else {
                    //先清理一下 如果商品无效的话
                    cartSkuVOS.remove(cartSkuVO);
                    //购物车中不存在此商品，则新建立一个
                    cartSkuVO = new CartSkuVO(dataSku);

                    cartSkuVO.setCartType(cartTypeEnum);
                    promotionGoodsService.updatePromotion(cartSkuVO);
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
                AuthUser currentUser = UserContext.getCurrentUser();
                tradeDTO.setMemberId(currentUser.getId());
                tradeDTO.setMemberName(currentUser.getUsername());
                List<CartSkuVO> cartSkuVOS = tradeDTO.getSkuList();

                //购物车中不存在此商品，则新建立一个
                CartSkuVO cartSkuVO = new CartSkuVO(dataSku);
                cartSkuVO.setCartType(cartTypeEnum);
                promotionGoodsService.updatePromotion(cartSkuVO);
                //再设置加入购物车的数量
                this.checkSetGoodsQuantity(cartSkuVO, skuId, num);
                //计算购物车小计
                cartSkuVO.setSubTotal(CurrencyUtil.mul(cartSkuVO.getPurchasePrice(), cartSkuVO.getNum()));
                cartSkuVOS.add(cartSkuVO);
            }
            tradeDTO.setCartTypeEnum(cartTypeEnum);
            // 如购物车发生更改，则重置优惠券
            tradeDTO.setStoreCoupons(null);
            tradeDTO.setPlatformCoupon(null);
            this.resetTradeDTO(tradeDTO);
        } catch (Exception e) {
            log.error("购物车渲染异常", e);
            throw new ServiceException(errorMessage);
        }
    }


    @Override
    public void updateNum(String skuId, int num) {
        try {
            checkGoods(skuId, num);
            TradeDTO tradeDTO = this.readDTO(CartTypeEnum.CART);
            CartSkuVO cartSkuVO = null;
            for (CartSkuVO skuVO : tradeDTO.getSkuList()) {
                if (skuVO.getGoodsSku().getId().equals(skuId)) {
                    cartSkuVO = skuVO;
                }
            }
            if (cartSkuVO != null) {
                this.checkSetGoodsQuantity(cartSkuVO, skuId, num);
            }
            String originKey = this.getOriginKey(CartTypeEnum.CART);
            cache.put(originKey, tradeDTO);
        } catch (ServiceException se) {
            log.error("购物车渲染异常", se);
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

        String cacheKey = "";
        //如果会员登录了，则要以会员id为key
        AuthUser currentUser = UserContext.getCurrentUser();
        if (cartTypeEnum.equals(CartTypeEnum.CART)) {
            cacheKey = TradeCacheEnum.CART_DATA.getPrefix() + currentUser.getId();
        } else if (cartTypeEnum.equals(CartTypeEnum.BUY_NOW)) {
            cacheKey = TradeCacheEnum.BUY_NOW_CART_DATA.getPrefix() + currentUser.getId();
        } else if (cartTypeEnum.equals(CartTypeEnum.PINTUAN)) {
            cacheKey = TradeCacheEnum.PINTUAN.getPrefix() + currentUser.getId();
        }
        return cacheKey;
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
        // 清除选择的优惠券
        tradeDTO.setPlatformCoupon(null);
        tradeDTO.setStoreCoupons(null);
        // 清除添加过的备注
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
        return tradeBuilder.buildTrade(way);
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
            List<String> ids = tradeDTO.getSkuList().parallelStream().filter(i -> Boolean.TRUE.equals(i.getChecked())).map(i -> i.getGoodsSku().getId()).collect(Collectors.toList());
            List<String> storeIds = new ArrayList<>();
            List<EsGoodsIndex> esGoodsList = esGoodsSearchService.getEsGoodsBySkuIds(ids);
            for (EsGoodsIndex esGoodsIndex : esGoodsList) {
                if (esGoodsIndex.getPromotionMap() != null) {
                    List<String> couponIds = esGoodsIndex.getPromotionMap().keySet().parallelStream().filter(i -> i.contains(PromotionTypeEnum.COUPON.name())).map(i -> i.substring(i.lastIndexOf("-") + 1)).collect(Collectors.toList());
                    if (!couponIds.isEmpty()) {
                        List<MemberCoupon> currentGoodsCanUse = memberCouponService.getCurrentGoodsCanUse(tradeDTO.getMemberId(), couponIds, totalPrice);
                        count = currentGoodsCanUse.size();
                    }
                }
                storeIds.add(esGoodsIndex.getStoreId());
            }
            List<MemberCoupon> allScopeMemberCoupon = memberCouponService.getAllScopeMemberCoupon(tradeDTO.getMemberId(), storeIds);
            if (allScopeMemberCoupon != null && !allScopeMemberCoupon.isEmpty()) {
                // 过滤满足消费门槛
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
     * 校验商品有效性，判定失效和库存
     *
     * @param skuId 商品skuId
     * @param num   数量
     */
    private GoodsSku checkGoods(String skuId, Integer num) {
        GoodsSku dataSku = this.goodsSkuService.getGoodsSkuByIdFromCache(skuId);
        if (dataSku == null) {
            throw new ServiceException("商品已失效，请重新选购！");
        }
        if (!GoodsAuthEnum.PASS.name().equals(dataSku.getIsAuth()) || !GoodsStatusEnum.UPPER.name().equals(dataSku.getMarketEnable())) {
            throw new ServiceException("商品已下架，请重新选购！");
        }
        //读取sku的可用库存
        Integer enableQuantity = goodsSkuService.getStock(skuId);

        //如果sku的可用库存小于等于0或者小于用户购买的数量，则不允许购买
        if (enableQuantity <= 0 || enableQuantity < num) {
            throw new ServiceException("商品库存已不足，请选购其他商品。");
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
        if (enableStock <= num) {
            cartSkuVO.setNum(enableStock);
        } else {
            cartSkuVO.setNum(num);
        }

        if (cartSkuVO.getNum() > 100) {
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
        TradeDTO tradeDTO = this.getCheckedTradeDTO(CartTypeEnum.CART);
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
    public void selectCoupon(String couponId, String way, boolean use) {
        //获取购物车，然后重新写入优惠券
        CartTypeEnum cartTypeEnum = getCartType(way);
        TradeDTO tradeDTO = this.readDTO(cartTypeEnum);

        MemberCoupon memberCoupon =
                memberCouponService.getOne(
                        new LambdaQueryWrapper<MemberCoupon>()
                                .eq(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.NEW.name())
                                .eq(MemberCoupon::getId, couponId));
        if (memberCoupon == null) {
            throw new ServiceException(ResultCode.COUPON_EXPIRED);
        }
        //使用优惠券 与否
        if (use && checkCoupon(memberCoupon, tradeDTO)) {
            this.useCoupon(tradeDTO, memberCoupon, cartTypeEnum);
        } else if (!use) {
            if (Boolean.TRUE.equals(memberCoupon.getIsPlatform())) {
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
        //过滤勾选商品
        List<CartSkuVO> collect = tradeDTO.getSkuList().parallelStream().filter(i -> Boolean.TRUE.equals(i.getChecked())).collect(Collectors.toList());
        //校验收获地址
        MemberAddress memberAddress = tradeDTO.getMemberAddress();
        this.checkAddressScope(collect, memberAddress);
        this.resetTradeDTO(tradeDTO);
        //构建交易
        Trade trade = tradeBuilder.createTrade(cartTypeEnum, tradeParams.getParentOrderSn());
        this.cleanChecked(tradeDTO);
        return trade;
    }

    /**
     * 检查商品是否在配送范围
     *
     * @param skuList       商品列表
     * @param memberAddress 配送地址
     */
    @Override
    public void checkAddressScope(List<CartSkuVO> skuList, MemberAddress memberAddress) {
        //如果收货地址为空，则抛出异常
        if (memberAddress == null) {
            throw new ServiceException(ResultCode.MEMBER_ADDRESS_NOT_EXIST);
        }
        for (CartSkuVO cartSkuVO : skuList) {
            String freightTemplateId = cartSkuVO.getGoodsSku().getFreightTemplateId();
            FreightTemplateVO freightTemplate = freightTemplateService.getFreightTemplate(freightTemplateId);
            //店铺支付运费则跳过
            if(freightTemplate.getPricingMethod().equals(FreightTemplateEnum.FREE.name())){
                break;
            }
            //收货地址判定
            forTemplates:
            if (freightTemplate != null && freightTemplate.getFreightTemplateChildList() != null && !freightTemplate.getFreightTemplateChildList().isEmpty()) {

                //获取市级别id
                String addressId = memberAddress.getConsigneeAddressIdPath().split(",")[1];
                //获取匹配的收货地址
                for (FreightTemplateChild templateChild : freightTemplate.getFreightTemplateChildList()) {
                    //如果当前模版包含，则返回
                    if (templateChild.getAreaId().contains(addressId)) {
                        break forTemplates;
                    }
                }
                throw new ServiceException(ResultCode.SHIPPING_NOT_APPLY, cartSkuVO.getGoodsSku().getGoodsName());

            }
        }
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

    private void useCoupon(TradeDTO tradeDTO, MemberCoupon memberCoupon, CartTypeEnum cartTypeEnum) {
        //如果是平台优惠券
        if (Boolean.TRUE.equals(memberCoupon.getIsPlatform())) {
            // 购物车价格
            Double cartPrice = 0d;
            for (CartSkuVO cartSkuVO : tradeDTO.getSkuList()) {
                // 获取商品的促销信息
                Optional<PromotionGoods> promotionOptional = cartSkuVO.getPromotions().parallelStream().filter(i -> (i.getPromotionType().equals(PromotionTypeEnum.PINTUAN.name()) && cartTypeEnum.equals(CartTypeEnum.PINTUAN))
                        || i.getPromotionType().equals(PromotionTypeEnum.SECKILL.name())).findAny();
                if (promotionOptional.isPresent()) {
                    cartPrice += CurrencyUtil.mul(promotionOptional.get().getPrice(), cartSkuVO.getNum());
                } else {
                    cartPrice += CurrencyUtil.mul(cartSkuVO.getGoodsSku().getPrice(), cartSkuVO.getNum());
                }

            }
            if (memberCoupon.getConsumeThreshold() <= cartPrice) {
                tradeDTO.setPlatformCoupon(new MemberCouponDTO(memberCoupon));
                tradeDTO.setStoreCoupons(new HashMap<>());
            }
        } else {
            CartSkuVO cartVO = tradeDTO.getSkuList().stream().filter(i -> i.getStoreId().equals(memberCoupon.getStoreId())).findFirst().orElse(null);
            // 优惠券消费门槛 <= 商品购买时的成交价（单品） * 购买数量
            if (cartVO != null && memberCoupon.getConsumeThreshold() <= CurrencyUtil.mul(cartVO.getPurchasePrice(), cartVO.getNum())) {
                tradeDTO.getStoreCoupons().put(memberCoupon.getStoreId(), new MemberCouponDTO(memberCoupon));
                tradeDTO.setPlatformCoupon(null);
            }
        }
    }

    /**
     * 校验是否可以使用优惠券
     *
     * @param memberCoupon 用于计算优惠券结算详情
     * @param tradeDTO     购物车信息
     * @return 是否可以使用优惠券
     */
    private boolean checkCoupon(MemberCoupon memberCoupon, TradeDTO tradeDTO) {
        List<CartSkuVO> cartSkuVOS;
        if (Boolean.FALSE.equals(memberCoupon.getIsPlatform())) {
            cartSkuVOS = tradeDTO.getSkuList().stream().filter(i -> i.getStoreId().equals(memberCoupon.getStoreId())).collect(Collectors.toList());
        } else {
            cartSkuVOS = tradeDTO.getSkuList();
        }

        // 当初购物车商品中是否存在符合优惠券条件的商品sku
        if (memberCoupon.getScopeType().equals(CouponScopeTypeEnum.PORTION_GOODS_CATEGORY.name())) {
            // 分类路径是否包含
            return cartSkuVOS.stream().anyMatch(i -> i.getGoodsSku().getCategoryPath().indexOf("," + memberCoupon.getScopeId() + ",") <= 0);
        } else if (memberCoupon.getScopeType().equals(CouponScopeTypeEnum.PORTION_GOODS.name())) {
            // 范围关联ID是否包含
            return cartSkuVOS.stream().anyMatch(i -> memberCoupon.getScopeId().indexOf("," + i.getGoodsSku().getId() + ",") <= 0);
        } else if (memberCoupon.getScopeType().equals(CouponScopeTypeEnum.PORTION_SHOP_CATEGORY.name())) {
            // 分类路径是否包含
            return cartSkuVOS.stream().anyMatch(i -> i.getGoodsSku().getStoreCategoryPath().indexOf("," + memberCoupon.getScopeId() + ",") <= 0);
        }
        return true;
    }

    @Autowired
    public void setTradeBuilder(TradeBuilder tradeBuilder) {
        this.tradeBuilder = tradeBuilder;
    }
}
