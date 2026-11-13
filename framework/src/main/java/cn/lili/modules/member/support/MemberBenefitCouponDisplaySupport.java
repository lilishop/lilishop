package cn.lili.modules.member.support;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.modules.member.entity.vo.MemberBenefitCouponItemVO;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.enums.CouponRangeDayEnum;
import cn.lili.modules.promotion.entity.enums.CouponTypeEnum;
import cn.lili.modules.promotion.service.CouponService;

import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 券礼包展示数据组装，关联当前优惠券信息并格式化面值和有效期，不参与权益发放。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class MemberBenefitCouponDisplaySupport {

    private final CouponService couponService;

    /**
     * 注入优惠券服务以补充券礼包展示信息。
     *
     * @param couponService 优惠券服务
     */
    public MemberBenefitCouponDisplaySupport(CouponService couponService) {
        this.couponService = couponService;
    }

    /**
     * 将券礼包配置转换为展示行，关联当前优惠券的名称、面值及有效期。
     * 配置空白或解析失败时返回空列表；优惠券缺失时保留配置中的主键和数量。
     *
     * @param benefitConfig 券礼包扩展配置JSON
     * @return 券礼包展示明细，顺序与配置数组一致
     */
    public List<MemberBenefitCouponItemVO> buildCouponPackageDisplayItems(String benefitConfig) {
        List<MemberBenefitCouponItemVO> list = new ArrayList<>();
        if (CharSequenceUtil.isBlank(benefitConfig)) {
            return list;
        }
        JSONObject obj;
        try {
            obj = JSONUtil.parseObj(benefitConfig);
        } catch (Exception e) {
            return list;
        }
        JSONArray coupons = obj.getJSONArray("coupons");
        if (coupons == null || coupons.isEmpty()) {
            return list;
        }
        for (int i = 0; i < coupons.size(); i++) {
            JSONObject item = coupons.getJSONObject(i);
            if (item == null) {
                continue;
            }
            String couponId = item.getStr("couponId");
            Integer quantity = this.parseCouponQuantity(item.get("quantity"));
            MemberBenefitCouponItemVO row = new MemberBenefitCouponItemVO();
            row.setCouponId(CharSequenceUtil.isNotBlank(couponId) ? couponId.trim() : null);
            row.setQuantity(quantity);
            if (CharSequenceUtil.isNotBlank(couponId)) {
                Coupon coupon = this.couponService.getById(couponId.trim());
                if (coupon != null) {
                    row.setCouponName(coupon.getCouponName());
                    row.setCouponType(coupon.getCouponType());
                    row.setPrice(coupon.getPrice());
                    row.setCouponDiscount(coupon.getCouponDiscount());
                    row.setFaceValueText(this.buildCouponFaceValueText(coupon));
                    row.setValidityText(this.buildCouponValidityText(coupon));
                }
            }
            list.add(row);
        }
        return list;
    }

    /**
     * 解析券礼包展示数量，支持数字及整数字符串，不在此校验业务范围。
     *
     * @param qtyRaw 配置中的原始quantity值
     * @return 解析后的数量，缺失或字符串解析失败时为null
     */
    private Integer parseCouponQuantity(Object qtyRaw) {
        if (qtyRaw == null) {
            return null;
        }
        try {
            if (qtyRaw instanceof Number) {
                return ((Number) qtyRaw).intValue();
            }
            return Integer.parseInt(qtyRaw.toString().trim());
        } catch (NumberFormatException e) {
            return null;
        }
    }

    /**
     * 格式化优惠券面值：现金券显示减免金额（元），折扣券显示折数。
     *
     * @param coupon 优惠券，可为null
     * @return 展示文本；缺失券或类型时为null，无法识别的面值为短横线
     */
    private String buildCouponFaceValueText(Coupon coupon) {
        if (coupon == null || coupon.getCouponType() == null) {
            return null;
        }
        if (CouponTypeEnum.PRICE.name().equals(coupon.getCouponType()) && coupon.getPrice() != null) {
            return "减免¥" + coupon.getPrice();
        }
        if (CouponTypeEnum.DISCOUNT.name().equals(coupon.getCouponType()) && coupon.getCouponDiscount() != null) {
            return coupon.getCouponDiscount() + "折";
        }
        return "-";
    }

    /**
     * 格式化优惠券有效期，固定区间显示起止时间，动态有效期显示领取后有效天数。
     *
     * @param coupon 优惠券，可为null
     * @return 有效期说明，信息不足时为null
     */
    private String buildCouponValidityText(Coupon coupon) {
        if (coupon == null || coupon.getRangeDayType() == null) {
            Date st = coupon != null ? coupon.getStartTime() : null;
            Date et = coupon != null ? coupon.getEndTime() : null;
            if (st != null || et != null) {
                String a = st != null ? DateUtil.formatDateTime(st) : "";
                String b = et != null ? DateUtil.formatDateTime(et) : "";
                if (CharSequenceUtil.isNotBlank(a) && CharSequenceUtil.isNotBlank(b)) {
                    return a + " ~ " + b;
                }
            }
            return null;
        }
        if (CouponRangeDayEnum.FIXEDTIME.name().equals(coupon.getRangeDayType())) {
            Date st = coupon.getStartTime();
            Date et = coupon.getEndTime();
            if (st != null && et != null) {
                return DateUtil.formatDateTime(st) + " ~ " + DateUtil.formatDateTime(et);
            }
            if (st != null) {
                return DateUtil.formatDateTime(st) + " 起";
            }
            if (et != null) {
                return "至 " + DateUtil.formatDateTime(et);
            }
        }
        if (CouponRangeDayEnum.DYNAMICTIME.name().equals(coupon.getRangeDayType())
                && coupon.getEffectiveDays() != null
                && coupon.getEffectiveDays() > 0) {
            return "领取后" + coupon.getEffectiveDays() + "天内有效";
        }
        return null;
    }
}
