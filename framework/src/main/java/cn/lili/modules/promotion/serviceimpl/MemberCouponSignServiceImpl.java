package cn.lili.modules.promotion.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.DateUtil;
import cn.lili.modules.promotion.entity.dos.CouponActivity;
import cn.lili.modules.promotion.entity.dos.MemberCouponSign;
import cn.lili.modules.promotion.entity.enums.CouponFrequencyEnum;
import cn.lili.modules.promotion.entity.vos.CouponActivityVO;
import cn.lili.modules.promotion.mapper.MemberCouponSignMapper;
import cn.lili.modules.promotion.service.MemberCouponSignService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

/**
 * 会员优惠券领取标记
 *
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.0
 * @Description:
 * @since 2023/1/3 18:13
 */
@Service
public class MemberCouponSignServiceImpl extends ServiceImpl<MemberCouponSignMapper, MemberCouponSign> implements MemberCouponSignService {


    @Autowired
    private Cache cache;

    @Override
    public void clean() {
        LambdaQueryWrapper<MemberCouponSign> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.lt(MemberCouponSign::getInvalidTime, DateUtil.getCurrentDayStartTime());
        this.baseMapper.delete(queryWrapper);
        cache.remove(CachePrefix.MEMBER_COUPON_SIGN.getPrefix());
    }

    @Override
    public List<CouponActivityVO> receiveCoupon(List<CouponActivityVO> couponActivity) {
        List<MemberCouponSign> memberCouponSigns = new ArrayList<>();


        //查询当前用户领取标记
        LambdaQueryWrapper<MemberCouponSign> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(MemberCouponSign::getMemberId, UserContext.getCurrentUser().getId());
        List<MemberCouponSign> oldMemberCouponSigns = this.list(queryWrapper);

        //定义生效的活动
        List<CouponActivityVO> assertCouponActivity = new ArrayList<>();

        //生成标记对象
        couponActivityFor:
        for (CouponActivityVO activity : couponActivity) {
            //如果旧的标记中包含记号，则略过
            for (MemberCouponSign oldMemberCouponSign : oldMemberCouponSigns) {
                if (oldMemberCouponSign.getCouponActivityId().equals(activity.getId())) {
                    continue couponActivityFor;
                }
            }
            assertCouponActivity.add(activity);

            MemberCouponSign memberCouponSign = new MemberCouponSign();
            memberCouponSign.setMemberId(UserContext.getCurrentUser().getId());
            memberCouponSign.setCouponActivityId(activity.getId());
            memberCouponSign.setInvalidTime(getInvalidTime(activity));
            memberCouponSigns.add(memberCouponSign);
        }

        this.saveBatch(memberCouponSigns);
        return assertCouponActivity;
    }

    /**
     * 根据活动优惠券获取标记失效时间
     *
     * @param activity
     * @return
     */
    private Date getInvalidTime(CouponActivity activity) {

        //领取周期符合预设
        if (CouponFrequencyEnum.exist(activity.getCouponFrequencyEnum())) {

            Calendar cal = DateUtil.getCleanCalendar();
            switch (CouponFrequencyEnum.valueOf(activity.getCouponFrequencyEnum())) {
                case DAY:
                    return DateUtil.getCurrentDayEndTime();
                case WEEK:
                    //周一
                    cal.set(Calendar.DAY_OF_WEEK, 2);
                    //去往下周
                    cal.set(Calendar.WEEK_OF_YEAR, cal.get(Calendar.WEEK_OF_YEAR) + 1);
                    //减1毫秒上个星期最后一刻
                    cal.set(Calendar.MILLISECOND, -1);
                    return cal.getTime();
                case MONTH:
                    //日期 1日
                    cal.set(Calendar.DAY_OF_MONTH, 1);
                    //下个月
                    cal.set(Calendar.MONTH, cal.get(Calendar.MONTH) + 1);
                    //减少一毫秒去上个月最后一刻
                    cal.set(Calendar.MILLISECOND, -1);

                    return cal.getTime();
                default:

                    throw new ServiceException();
            }
        } else {
            throw new ServiceException();
        }
    }


}