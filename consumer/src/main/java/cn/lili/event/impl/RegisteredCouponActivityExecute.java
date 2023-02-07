package cn.lili.event.impl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.event.MemberRegisterEvent;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.promotion.entity.dto.CouponActivityTrigger;
import cn.lili.modules.promotion.entity.enums.CouponActivityTypeEnum;
import cn.lili.modules.promotion.service.CouponActivityService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 注册赠券活动
 *
 * @author Bulbasaur
 * @since 2021/5/24 10:48 上午
 */
@Component
public class RegisteredCouponActivityExecute implements MemberRegisterEvent {

    @Autowired
    private CouponActivityService couponActivityService;


    @Autowired
    private MemberService memberService;
    @Autowired
    private Cache cache;

    /**
     * 获取进行中的注册赠券的优惠券活动
     * 发送注册赠券
     *
     * @param member 会员
     */
    @Override
    public void memberRegister(Member member) {
        //用户注册赠券
        couponActivityService.trigger(CouponActivityTrigger.builder()
                .nickName(member.getNickName())
                .userId(member.getId())
                .couponActivityTypeEnum(CouponActivityTypeEnum.REGISTERED)
                .build());
        //邀请人赠券
        String memberId = (String) cache.get(CachePrefix.INVITER.getPrefix() + member.getId());
        if (CharSequenceUtil.isNotEmpty(memberId)) {
            //邀请人
            Member inviter = memberService.getById(memberId);
            couponActivityService.trigger(CouponActivityTrigger.builder()
                    .nickName(inviter.getNickName())
                    .userId(inviter.getId())
                    .couponActivityTypeEnum(CouponActivityTypeEnum.INVITE_NEW)
                    .build());
        }
    }
}
