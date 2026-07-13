package cn.lili.modules.promotion.entity.dto.search;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.promotion.entity.enums.CouponGetEnum;
import cn.lili.modules.promotion.entity.enums.CouponTypeEnum;
import cn.lili.modules.promotion.entity.enums.MemberCouponStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;
import java.util.Date;

/**
 * 客户优惠券查询通用类
 *
 * @author paulG
 * @since 2020/8/14
 **/
@EqualsAndHashCode(callSuper = true)
@Data
public class MemberCouponSearchParams extends BasePromotionsSearchParams implements Serializable {

    private static final long serialVersionUID = 4566880169478260409L;

    private static final String PRICE_COLUMN = "price";

    @Schema(description = "优惠券id")
    private String couponId;

    @Schema(description = "优惠券名称")
    private String couponName;

    @Schema(description = "客户id")
    private String memberId;

    @Schema(description = "客户名称")
    private String memberName;

    /**
     * POINT("打折"), PRICE("减免现金");
     *
     * @see CouponTypeEnum
     */
    @Schema(description = "活动类型")
    private String couponType;
    /**
     * @see PromotionsScopeTypeEnum
     */
    @Schema(description = "关联范围类型")
    private String scopeType;
    @Schema(description = "范围关联的id")
    private String scopeId;
    @Schema(description = "面额,可以为范围，如10_1000")
    private String price;
    /**
     * @see CouponGetEnum
     */
    @Schema(description = "优惠券类型，分为免费领取和活动赠送")
    private String getType;
    /**
     * @see MemberCouponStatusEnum
     */
    @Schema(description = "客户优惠券状态")
    private String memberCouponStatus;
    @Schema(description = "消费门槛")
    private Double consumeThreshold;

    @Schema(description = "领取来源直播间ID")
    private String liveRoomId;


    @Override
    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = super.queryWrapper();
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(couponId), "coupon_id", couponId);
        queryWrapper.like(CharSequenceUtil.isNotEmpty(couponName), "coupon_name", couponName);
        queryWrapper.like(CharSequenceUtil.isNotEmpty(memberName), "member_name", memberName);
        if (CharSequenceUtil.isNotEmpty(couponType)) {
            queryWrapper.eq("coupon_type", CouponTypeEnum.valueOf(couponType).name());
        }
        if (memberId != null) {
            queryWrapper.eq("member_id", memberId);
        }
        if (CharSequenceUtil.isNotEmpty(scopeId)) {
            queryWrapper.eq("scope_id", scopeId);
        }
        if (CharSequenceUtil.isNotEmpty(scopeType)) {
            queryWrapper.eq("scope_type", PromotionsScopeTypeEnum.valueOf(scopeType).name());
        }
        if (CharSequenceUtil.isNotEmpty(getType)) {
            queryWrapper.eq("get_type", CouponGetEnum.valueOf(getType).name());
        }
        if (CharSequenceUtil.isNotEmpty(memberCouponStatus)) {
            queryWrapper.eq("member_coupon_status", MemberCouponStatusEnum.valueOf(memberCouponStatus).name());
        }
        if (CharSequenceUtil.isNotEmpty(liveRoomId)) {
            queryWrapper.eq("live_room_id", liveRoomId);
        }
        if (CharSequenceUtil.isNotEmpty(price)) {
            String[] s = price.split("_");
            if (s.length > 1) {
                queryWrapper.between(PRICE_COLUMN, s[0], s[1]);
            } else {
                queryWrapper.ge(PRICE_COLUMN, s[0]);
            }
        }
        if (this.getStartTime() != null) {
            queryWrapper.ge("start_time", new Date(this.getEndTime()));
        }
        if (this.getEndTime() != null) {
            queryWrapper.le("end_time", new Date(this.getEndTime()));
        }
        queryWrapper.eq("delete_flag", false);
        queryWrapper.orderByDesc("create_time");
        return queryWrapper;
    }


}
