package cn.lili.timetask.handler.impl.coupon;

import cn.hutool.core.date.DateUtil;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.enums.MemberCouponStatusEnum;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.timetask.handler.EveryDayExecute;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 优惠券状态监测
 *
 * @author Bulbasaur
 * @date: 2021/5/24 10:08 上午
 */
@Component
public class CouponExecute implements EveryDayExecute {

    @Autowired
    private MemberCouponService memberCouponService;

    /**
     * 检测优惠券的使用时间，超期未使用则失效
     * 此方法用于领取*天后失效优惠券使用
     */
    @Override
    public void execute() {
        LambdaUpdateWrapper<MemberCoupon> updateWrapper = new LambdaUpdateWrapper<MemberCoupon>()
                .eq(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.NEW.name())
                .le(MemberCoupon::getEndTime, DateUtil.date())
                .set(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.EXPIRE.name());
        this.memberCouponService.update(updateWrapper);

    }

}
