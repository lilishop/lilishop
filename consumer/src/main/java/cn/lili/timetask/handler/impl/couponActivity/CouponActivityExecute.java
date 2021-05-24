package cn.lili.timetask.handler.impl.couponActivity;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.lili.modules.promotion.entity.dos.CouponActivity;
import cn.lili.modules.promotion.entity.enums.CouponActivityTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.service.CouponActivityService;
import cn.lili.timetask.handler.EveryMinuteExecute;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 优惠券活动状态监测
 *
 * @author Bulbasaur
 * @date: 2021/5/24 10:08 上午
 */
@Component
public class CouponActivityExecute implements EveryMinuteExecute {

    @Autowired
    private CouponActivityService couponActivityService;

    @Override
    public void execute() {
        //精确发券活动
        specify();
        //注册赠券的活动
        registered();
    }

    /**
     * 监测精确发券活动
     * 达到活动时间发送优惠券
     */
    private void specify(){
        DateTime dateTime=DateUtil.date();
        List<CouponActivity> couponActivities=couponActivityService.list(new LambdaQueryWrapper<CouponActivity>()
                .eq(CouponActivity::getCouponActivityType, CouponActivityTypeEnum.SPECIFY.name())
                .le(CouponActivity::getStartTime, dateTime)
                .eq(CouponActivity::getPromotionStatus,PromotionStatusEnum.NEW.name()));
        //如果有符合要求的优惠券活动，发送优惠券
        if(couponActivities.size()>0){
            for (CouponActivity CouponActivity:couponActivities) {
                couponActivityService.specify(CouponActivity.getId());
            }
            //修改精准发券优惠券活动状态
            couponActivityService.update(new LambdaUpdateWrapper<CouponActivity>()
                    .eq(CouponActivity::getCouponActivityType, CouponActivityTypeEnum.SPECIFY.name())
                    .set(CouponActivity::getPromotionStatus,PromotionStatusEnum.END.name()));
        }

    }
    /**
     * 注册赠券活动状态监测
     */
    private void registered(){
        //开启注册赠券优惠券活动
        LambdaUpdateWrapper<CouponActivity> lambdaUpdateWrapper = new LambdaUpdateWrapper<>();
        lambdaUpdateWrapper.eq(CouponActivity::getCouponActivityType, CouponActivityTypeEnum.REGISTERED.name())
                .eq(CouponActivity::getPromotionStatus, PromotionStatusEnum.NEW.name())
                .le(CouponActivity::getStartTime, DateUtil.date())
                .set(CouponActivity::getActivityScope,PromotionStatusEnum.START.name());
        couponActivityService.update(lambdaUpdateWrapper);

        //关闭注册赠券优惠券活动
        LambdaUpdateWrapper<CouponActivity> endWrapper = new LambdaUpdateWrapper<>();
        endWrapper.eq(CouponActivity::getCouponActivityType, CouponActivityTypeEnum.REGISTERED.name())
                .eq(CouponActivity::getPromotionStatus, PromotionStatusEnum.START.name())
                .le(CouponActivity::getEndTime, DateUtil.date())
                .set(CouponActivity::getActivityScope,PromotionStatusEnum.END.name());
        couponActivityService.update(endWrapper);
    }
}
