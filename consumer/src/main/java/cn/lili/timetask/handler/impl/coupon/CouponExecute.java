package cn.lili.timetask.handler.impl.coupon;

import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.enums.MemberCouponStatusEnum;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.timetask.handler.EveryDayExecute;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Calendar;
import java.util.Date;

/**
 * 优惠券状态监测
 *
 * @author Bulbasaur
 * @since 2021/5/24 10:08 上午
 */
@Component
public class CouponExecute implements EveryDayExecute {

    /**
     * 过期常量，过期后或者使用后一定时间内，删除无效的优惠券，物理删除
     */
    static final int EXPIRATION_DAY = 3;

    @Autowired
    private MemberCouponService memberCouponService;

    /**
     * 检测优惠券的使用时间，超期未使用则失效
     * 此方法用于领取*天后失效优惠券使用
     */
    @Override
    public void execute() {
        //将过期优惠券变更为过期状态
        LambdaUpdateWrapper<MemberCoupon> updateWrapper = new LambdaUpdateWrapper<MemberCoupon>()
                .eq(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.NEW.name())
                .le(MemberCoupon::getEndTime, new Date())
                .set(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.EXPIRE.name());
        this.memberCouponService.update(updateWrapper);

        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.DAY_OF_MONTH, calendar.get(Calendar.DAY_OF_MONTH) - EXPIRATION_DAY);
        Date removeTime = calendar.getTime();
        //删除过期/已使用的优惠券
        LambdaUpdateWrapper<MemberCoupon> deleteWrapper = new LambdaUpdateWrapper<MemberCoupon>()
                //如果结束时间小于 当前时间增加指定删除日期，则删除
                .le(MemberCoupon::getEndTime, removeTime);
        this.memberCouponService.remove(deleteWrapper);


    }

}
