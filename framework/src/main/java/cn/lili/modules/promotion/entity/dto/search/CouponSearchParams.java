package cn.lili.modules.promotion.entity.dto.search;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.promotion.entity.enums.*;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;
import java.util.Date;

/**
 * 优惠券查询通用类
 *
 * @author paulG
 * @since 2020/8/14
 **/
@EqualsAndHashCode(callSuper = true)
@Data
public class CouponSearchParams extends BasePromotionsSearchParams implements Serializable {

    private static final long serialVersionUID = 4566880169478260409L;

    private static final String PRICE_COLUMN = "price";
    private static final String RANGE_DAY_TYPE_COLUMN = "range_day_type";

    @ApiModelProperty(value = "会员id")
    private String memberId;

    @ApiModelProperty(value = "优惠券名称")
    private String couponName;
    /**
     * POINT("打折"), PRICE("减免现金");
     *
     * @see cn.lili.modules.promotion.entity.enums.CouponTypeEnum
     */
    @ApiModelProperty(value = "活动类型")
    private String couponType;
    /**
     * @see PromotionsScopeTypeEnum
     */
    @ApiModelProperty(value = "关联范围类型")
    private String scopeType;
    @ApiModelProperty(value = "范围关联的id")
    private String scopeId;
    @ApiModelProperty(value = "面额,可以为范围，如10_1000")
    private String price;
    @ApiModelProperty(value = "发行数量,可以为范围，如10_1000")
    private String publishNum;
    @ApiModelProperty(value = "已被领取的数量,可以为范围，如10_1000")
    private String receivedNum;
    /**
     * @see cn.lili.modules.promotion.entity.enums.CouponGetEnum
     */
    @ApiModelProperty(value = "优惠券类型，分为免费领取和活动赠送")
    private String getType;


    @Override
    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = super.baseQueryWrapper();
        if (CharSequenceUtil.isNotEmpty(couponName)) {
            queryWrapper.like("coupon_name", couponName);
        }
        if (memberId != null) {
            queryWrapper.eq("member_id", memberId);
        }
        if (CharSequenceUtil.isNotEmpty(couponType)) {
            queryWrapper.eq("coupon_type", CouponTypeEnum.valueOf(couponType).name());
        }
        if (CharSequenceUtil.isNotEmpty(scopeType)) {
            queryWrapper.eq("scope_type", PromotionsScopeTypeEnum.valueOf(scopeType).name());
        }
        if (CharSequenceUtil.isNotEmpty(scopeId)) {
            queryWrapper.eq("scope_id", scopeId);
        }
        if (CharSequenceUtil.isNotEmpty(getType)) {
            queryWrapper.eq("get_type", CouponGetEnum.valueOf(getType).name());
        }
        if (CharSequenceUtil.isNotEmpty(this.getPromotionStatus())) {
            queryWrapper.and(p -> {
                switch (PromotionsStatusEnum.valueOf(this.getPromotionStatus())) {
                    case NEW:
                        p.nested(i -> i.gt(PromotionTools.START_TIME_COLUMN, new Date()).gt(PromotionTools.END_TIME_COLUMN, new Date()));
                        break;
                    case START:
                        p.nested(i -> i.le(PromotionTools.START_TIME_COLUMN, new Date()).ge(PromotionTools.END_TIME_COLUMN, new Date()))
                                .or(i -> i.gt("effective_days", 0).eq(RANGE_DAY_TYPE_COLUMN, CouponRangeDayEnum.DYNAMICTIME.name()));
                        break;
                    case END:
                        p.nested(i -> i.lt(PromotionTools.START_TIME_COLUMN, new Date()).lt(PromotionTools.END_TIME_COLUMN, new Date()));
                        break;
                    case CLOSE:
                        p.nested(n -> n.nested(i -> i.isNull(PromotionTools.START_TIME_COLUMN).isNull(PromotionTools.END_TIME_COLUMN)
                                        .eq(RANGE_DAY_TYPE_COLUMN, CouponRangeDayEnum.FIXEDTIME.name())).
                                or(i -> i.le("effective_days", 0).eq(RANGE_DAY_TYPE_COLUMN, CouponRangeDayEnum.DYNAMICTIME.name())));
                        break;
                    default:
                }
            });

        }
        if (this.getStartTime() != null) {
            queryWrapper.ge("start_time", new Date(this.getEndTime()));
        }
        if (this.getEndTime() != null) {
            queryWrapper.le("end_time", new Date(this.getEndTime()));
        }
        this.betweenWrapper(queryWrapper);
        queryWrapper.orderByDesc("create_time");
        return queryWrapper;
    }

    private <T> void betweenWrapper(QueryWrapper<T> queryWrapper) {
        if (CharSequenceUtil.isNotEmpty(publishNum)) {
            String[] s = publishNum.split("_");
            if (s.length > 1) {
                queryWrapper.between("publish_num", s[0], s[1]);
            } else {
                queryWrapper.ge("publish_num", s[0]);
            }
        }
        if (CharSequenceUtil.isNotEmpty(price)) {
            String[] s = price.split("_");
            if (s.length > 1) {
                queryWrapper.between(PRICE_COLUMN, s[0], s[1]);
            } else {
                queryWrapper.ge(PRICE_COLUMN, s[0]);
            }
        }
        if (CharSequenceUtil.isNotEmpty(receivedNum)) {
            String[] s = receivedNum.split("_");
            if (s.length > 1) {
                queryWrapper.between("received_num", s[0], s[1]);
            } else {
                queryWrapper.ge("received_num", s[0]);
            }
        }
    }

}
