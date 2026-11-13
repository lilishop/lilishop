package cn.lili.modules.order.cart.support;

import cn.lili.cache.Cache;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.member.service.MemberAddressService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import com.alibaba.fastjson2.JSONException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.serializer.SerializationException;
import org.springframework.stereotype.Component;

/**
 * 购物车交易上下文缓存读写与读取初始化，供购物车服务和交易构造器共用，避免两者相互回调。
 * 沿用购买方式加当前会员ID的缓存键，读取失败保留原缓存；写入及清理不参与数据库事务回滚。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Slf4j
@Component
public class CartTradeCacheSupport {

    private final Cache<Object> cache;
    private final MemberAddressService memberAddressService;
    private final CardKeyService cardKeyService;
    private final AppointmentService appointmentService;

    /**
     * 注入缓存、默认地址及特殊商品交易归一化能力，不依赖购物车服务或交易构造器。
     *
     * @param cache 购物车缓存
     * @param memberAddressService 会员地址服务
     * @param cardKeyService 电子卡券交易服务
     * @param appointmentService 预约商品交易服务
     */
    public CartTradeCacheSupport(Cache<Object> cache, MemberAddressService memberAddressService,
                                CardKeyService cardKeyService, AppointmentService appointmentService) {
        this.cache = cache;
        this.memberAddressService = memberAddressService;
        this.cardKeyService = cardKeyService;
        this.appointmentService = appointmentService;
    }

    /**
     * 读取当前会员的交易上下文，未命中时创建空上下文；普通交易缺省时补充默认地址。
     * 电子卡券和预约商品沿用原有免默认地址及交易归一化规则，不主动回写缓存。
     * 反序列化失败时保留原缓存并按购物车或结算场景提示，其他运行时异常继续抛出。
     *
     * @param checkedWay 购买方式，不可为null
     * @return 初始化后的交易上下文
     */
    public TradeDTO readDTO(CartTypeEnum checkedWay) {
        String cacheKey = getOriginKey(checkedWay);
        TradeDTO tradeDTO;
        try {
            tradeDTO = (TradeDTO) cache.get(cacheKey);
        } catch (RuntimeException ex) {
            if (!isCacheSerializationFailure(ex)) {
                throw ex;
            }
            log.error("购物车缓存反序列化失败，保留原缓存等待修复, key={}", cacheKey, ex);
            String message = checkedWay == CartTypeEnum.CART
                    ? "购物车数据读取失败，请刷新重试"
                    : "结算信息已失效，请重新购买";
            throw new ServiceException(ResultCode.CART_ERROR, message);
        }
        if (tradeDTO == null) {
            tradeDTO = new TradeDTO(checkedWay);
            AuthUser currentUser = UserContext.getCurrentUser();
            tradeDTO.setMemberId(currentUser.getId());
            tradeDTO.setMemberName(currentUser.getUsername());
        }
        if (tradeDTO.getMemberAddress() == null
                && !cardKeyService.isECouponOnlyTrade(tradeDTO)
                && !appointmentService.isAppointmentOnlyTrade(tradeDTO)) {
            tradeDTO.setMemberAddress(memberAddressService.getDefaultMemberAddress());
        }
        if (cardKeyService.isECouponOnlyTrade(tradeDTO)) {
            cardKeyService.normalizeECouponTrade(tradeDTO);
        }
        if (appointmentService.isAppointmentOnlyTrade(tradeDTO)) {
            appointmentService.normalizeAppointmentTrade(tradeDTO);
        }
        return tradeDTO;
    }

    /**
     * 按上下文的购买方式覆盖当前会员缓存，沿用缓存默认有效期。
     *
     * @param tradeDTO 待保存的交易上下文
     */
    public void resetTradeDTO(TradeDTO tradeDTO) {
        cache.put(getOriginKey(tradeDTO.getCartTypeEnum()), tradeDTO);
    }

    /**
     * 清理当前会员的普通购物车缓存，不影响其他购买方式的结算上下文。
     */
    public void cleanCart() {
        cache.remove(getOriginKey(CartTypeEnum.CART));
    }

    /**
     * 按购买方式前缀和当前登录会员主键生成原有缓存键。
     *
     * @param cartTypeEnum 购买方式
     * @return 当前会员的交易上下文缓存键
     * @throws ServiceException 购买方式为null时抛出
     */
    private String getOriginKey(CartTypeEnum cartTypeEnum) {
        if (cartTypeEnum != null) {
            AuthUser currentUser = UserContext.getCurrentUser();
            return cartTypeEnum.getPrefix() + currentUser.getId();
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    /**
     * 沿异常原因链识别原有JSON或Redis反序列化异常，不将其他缓存故障视为数据格式错误。
     *
     * @param throwable 缓存读取异常
     * @return 原因链包含反序列化异常时为true
     */
    private boolean isCacheSerializationFailure(Throwable throwable) {
        Throwable current = throwable;
        while (current != null) {
            if (current instanceof JSONException || current instanceof SerializationException) {
                return true;
            }
            current = current.getCause();
        }
        return false;
    }
}
