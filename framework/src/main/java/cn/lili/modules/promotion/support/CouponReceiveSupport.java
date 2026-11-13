package cn.lili.modules.promotion.support;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.mapper.CouponMapper;
import org.springframework.stereotype.Component;

/**
 * 优惠券领取基础能力，共用优惠券读取和已领数量的原子更新，避免券活动与会员券服务相互依赖。
 * 沿用调用方事务及发行量校验SQL，不负责会员券落库或用户领取资格校验。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class CouponReceiveSupport {

    private final CouponMapper mapper;

    /**
     * 注入优惠券存储，不依赖会员券服务。
     *
     * @param mapper 优惠券存储
     */
    public CouponReceiveSupport(CouponMapper mapper) {
        this.mapper = mapper;
    }

    /**
     * 按主键读取优惠券，沿用存储层逻辑删除规则，不额外过滤有效期或活动状态。
     *
     * @param couponId 优惠券主键
     * @return 优惠券，不存在时为null
     */
    public Coupon getById(String couponId) {
        return mapper.selectById(couponId);
    }

    /**
     * 校验券存在且领取数量为正后，原子增加已领数量，发行量为零时不限量。
     * 有限发行时由原SQL保证已领数量加本次领取数量不超过发行量，更新失败抛出库存不足异常。
     * 沿用调用方事务；会员领券时与会员券记录写入共同提交或回滚。
     *
     * @param couponId 优惠券主键
     * @param receiveNum 本次领取张数，必须大于零
     * @throws ServiceException 券不存在、数量不合法或剩余发行量不足时抛出
     */
    public void receiveCoupon(String couponId, Integer receiveNum) {
        Coupon coupon = getById(couponId);
        if (coupon == null) {
            throw new ServiceException(ResultCode.COUPON_NOT_EXIST);
        }
        if (receiveNum == null || receiveNum <= 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        if (mapper.increaseReceivedNum(couponId, receiveNum) == 0) {
            throw new ServiceException(ResultCode.COUPON_NUM_INSUFFICIENT_ERROR);
        }
    }
}
