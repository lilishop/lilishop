package cn.lili.modules.promotion.entity.vos;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.promotion.entity.enums.*;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Date;
import java.util.regex.Pattern;

/**
 * 优惠券查询通用类
 *
 * @author paulG
 * @date 2020/8/14
 **/
@Data
public class CouponSearchParams implements Serializable {

    private static final long serialVersionUID = 4566880169478260409L;
    private static final String PRICE_COLUMN = "price";

    @ApiModelProperty(value = "店铺编号")
    private String storeId;

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
     * @see cn.lili.modules.promotion.entity.enums.CouponScopeTypeEnum
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

    @ApiModelProperty(value = "活动开始时间")
    private Long startTime;

    @ApiModelProperty(value = "活动结束时间")
    private Long endTime;
    /**
     * @see MemberCouponStatusEnum
     */
    @ApiModelProperty(value = "会员优惠券状态")
    private String memberCouponStatus;
    /**
     * @see PromotionStatusEnum
     */
    @ApiModelProperty(value = "活动状态")
    private String promotionStatus;

    public <T> QueryWrapper<T> wrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        if (storeId != null) {
            queryWrapper.in("store_id", Arrays.asList(storeId));
        }
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
            queryWrapper.eq("scope_type", CouponScopeTypeEnum.valueOf(scopeType).name());
        }
        if (CharSequenceUtil.isNotEmpty(scopeId)) {
            queryWrapper.eq("scope_id", scopeId);
        }
        if (CharSequenceUtil.isNotEmpty(getType)) {
            queryWrapper.eq("get_type", CouponGetEnum.valueOf(getType).name());
        }
        if (startTime != null) {
            queryWrapper.ge("start_time", new Date(startTime));
        }
        if (endTime != null) {
            queryWrapper.le("end_time", new Date(endTime));
        }
        if (CharSequenceUtil.isNotEmpty(memberCouponStatus)) {
            queryWrapper.eq("member_coupon_status", MemberCouponStatusEnum.valueOf(memberCouponStatus).name());
        }
        if (CharSequenceUtil.isNotEmpty(promotionStatus)) {
            queryWrapper.eq("promotion_status", PromotionStatusEnum.valueOf(promotionStatus).name());
        }
        this.betweenWrapper(queryWrapper);
        queryWrapper.eq("delete_flag", false);
        queryWrapper.orderByDesc("create_time");
        return queryWrapper;
    }

    private <T> void betweenWrapper(QueryWrapper<T> queryWrapper) {
        if (CharSequenceUtil.isNotEmpty(publishNum)) {
            String[] s = publishNum.split("_");
            if (s.length > 1) {
                queryWrapper.ge("publish_num", s[1]);
            } else {
                queryWrapper.le("publish_num", publishNum);
            }
        }
        if (CharSequenceUtil.isNotEmpty(price)) {
            String[] s = price.split("_");
            if (s.length > 1) {
                queryWrapper.ge(PRICE_COLUMN, s[1]);
            } else {
                queryWrapper.le(PRICE_COLUMN, s[0]);
            }
        }
        if (CharSequenceUtil.isNotEmpty(receivedNum)) {
            String[] s = receivedNum.split("_");
            if (s.length > 1) {
                queryWrapper.ge("received_num", s[1]);
            } else {
                queryWrapper.le("received_num", s[0]);
            }
        }
    }

    public Query mongoQuery() {
        Query query = new Query();
        if (storeId != null) {
            query.addCriteria(Criteria.where("storeId").in(Arrays.asList(storeId)));
        }
        if (CharSequenceUtil.isNotEmpty(couponName)) {
            Pattern pattern = Pattern.compile("^.*" + couponName + ".*$", Pattern.CASE_INSENSITIVE);
            query.addCriteria(Criteria.where("couponName").regex(pattern));
        }
        if (memberId != null) {
            query.addCriteria(Criteria.where("memberId").is(memberId));
        }
        if (CharSequenceUtil.isNotEmpty(couponType)) {
            query.addCriteria(Criteria.where("couponType").is(CouponTypeEnum.valueOf(couponType).name()));
        }
        if (CharSequenceUtil.isNotEmpty(scopeType)) {
            query.addCriteria(Criteria.where("scopeType").is(CouponScopeTypeEnum.valueOf(scopeType).name()));
        }
        if (CharSequenceUtil.isNotEmpty(scopeId)) {
            query.addCriteria(Criteria.where("scopeId").is(scopeId));
        }
        if (CharSequenceUtil.isNotEmpty(getType)) {
            query.addCriteria(Criteria.where("getType").is(CouponGetEnum.valueOf(getType).name()));
        }
        if (startTime != null) {
            query.addCriteria(Criteria.where("startTime").gte(new Date(startTime)));
        }
        if (endTime != null) {
            query.addCriteria(Criteria.where("endTime").lte(new Date(endTime)));
        }
        if (CharSequenceUtil.isNotEmpty(memberCouponStatus)) {
            query.addCriteria(Criteria.where("memberCouponStatus").is(MemberCouponStatusEnum.valueOf(memberCouponStatus).name()));
        }
        if (CharSequenceUtil.isNotEmpty(promotionStatus)) {
            query.addCriteria(Criteria.where("promotionStatus").is(PromotionStatusEnum.valueOf(promotionStatus).name()));
        }
        query.addCriteria(Criteria.where("deleteFlag").is(false));
        betweenQuery(query);
        return query;
    }

    private void betweenQuery(Query query) {
        if (CharSequenceUtil.isNotEmpty(price)) {
            String[] s = price.split("_");
            if (s.length > 1) {
                query.addCriteria(Criteria.where(PRICE_COLUMN).gt(s[1]));
            } else {
                query.addCriteria(Criteria.where(PRICE_COLUMN).lt(s[0]));
            }
        }
        if (CharSequenceUtil.isNotEmpty(publishNum)) {
            String[] s = publishNum.split("_");
            if (s.length > 1) {
                query.addCriteria(Criteria.where("publishNum").gt(s[1]));
            } else {
                query.addCriteria(Criteria.where("publishNum").lt(s[0]));
            }
        }
        if (CharSequenceUtil.isNotEmpty(receivedNum)) {
            String[] s = receivedNum.split("_");
            if (s.length > 1) {
                query.addCriteria(Criteria.where("receivedNum").gt(s[1]));
            } else {
                query.addCriteria(Criteria.where("receivedNum").lt(s[0]));
            }
        }
    }

}
