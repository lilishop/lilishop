package cn.lili.modules.member.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.entity.enums.MemberBenefitTypeEnum;
import cn.lili.modules.member.util.MemberBenefitRules;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.service.CouponService;

import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.HashSet;
import java.util.Set;

/**
 * 权益配置校验，集中检查必填字段、数值精度、发放周期及平台券归属；沿用公共权益规则。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class MemberBenefitConfigSupport {

    /**
     * 赠送积分权益：benefitConfig(JSON) 中 giftPoint 字段最大值
     */
    private static final int GIFT_POINT_CONFIG_MAX = 99999;

    /**
     * 券礼包每张优惠券单次赠送份数下限，单位张。
     */
    private static final int COUPON_PKG_QTY_MIN = 1;
    /**
     * 券礼包每张优惠券单次赠送份数上限，单位张。
     */
    private static final int COUPON_PKG_QTY_MAX = 10;

    private final CouponService couponService;

    /**
     * 注入平台券查询服务，用于保存配置时校验优惠券归属。
     *
     * @param couponService 优惠券服务
     */
    public MemberBenefitConfigSupport(CouponService couponService) {
        this.couponService = couponService;
    }

    /**
     * 校验权益必填信息、类型、排序、状态及类型专属配置，并清理说明富文本。
     * 积分倍率范围1～100且最多两位小数，赠送积分为0～99999整数，券礼包另校验周期及平台券。
     *
     * @param memberBenefit 待保存权益，说明会原地替换为清理后的内容
     * @throws ServiceException 必填信息、范围、精度或类型配置非法
     */
    public void validateBenefit(MemberBenefit memberBenefit) {
        if (memberBenefit == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "客户权益参数不能为空");
        }
        if (CharSequenceUtil.isBlank(memberBenefit.getBenefitName())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益名称不能为空");
        }
        if (!MemberBenefitTypeEnum.isValid(memberBenefit.getBenefitType())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益类型非法");
        }
        if (memberBenefit.getBenefitSort() == null || memberBenefit.getBenefitSort() < 1) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益排序必须为正整数");
        }
        if (!SwitchEnum.OPEN.name().equals(memberBenefit.getBenefitState()) && !SwitchEnum.CLOSE.name().equals(memberBenefit.getBenefitState())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益状态非法");
        }

        if (CharSequenceUtil.isBlank(memberBenefit.getBenefitLogo()))
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益图标不能为空");
        if (CharSequenceUtil.isBlank(memberBenefit.getBenefitDesc()))
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益介绍不能为空");
        memberBenefit.setBenefitDesc(MemberBenefitRules.description(memberBenefit.getBenefitDesc()));
        JSONObject config = MemberBenefitRules.config(memberBenefit.getBenefitConfig());
        if (MemberBenefitTypeEnum.POINT_MULTIPLIER.name().equals(memberBenefit.getBenefitType())) {
            MemberBenefitRules.number(config, "multiplier", BigDecimal.ONE, BigDecimal.valueOf(100), 2);
        }
        if (MemberBenefitTypeEnum.COUPON_PACKAGE.name().equals(memberBenefit.getBenefitType())) {
            MemberBenefitRules.validateSchedule(config);
        }
        if (MemberBenefitTypeEnum.GIFT_POINT.name().equals(memberBenefit.getBenefitType())) {
            MemberBenefitRules.number(config, "giftPoint", BigDecimal.ZERO, BigDecimal.valueOf(99999), 0);
            this.validateGiftPointConfig(memberBenefit.getBenefitConfig());
        }
        if (MemberBenefitTypeEnum.COUPON_PACKAGE.name().equals(memberBenefit.getBenefitType())) {
            this.validateCouponPackageConfig(memberBenefit.getBenefitConfig());
        }
    }

    /**
     * 校验赠送积分扩展配置包含giftPoint，范围0～99999。
     * 调用前由通用数值校验保证整数精度，本方法继续校验配置存在性及积分范围。
     *
     * @param benefitConfig 权益扩展配置JSON
     * @throws ServiceException 配置为空、JSON非法、缺少giftPoint或积分不合法
     */
    private void validateGiftPointConfig(String benefitConfig) {
        if (CharSequenceUtil.isBlank(benefitConfig)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "赠送积分权益需填写赠送积分数量（扩展配置 JSON 中设置 giftPoint）");
        }
        JSONObject obj;
        try {
            obj = JSONUtil.parseObj(benefitConfig);
        } catch (Exception e) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "赠送积分权益的扩展配置须为合法 JSON");
        }
        if (obj == null || obj.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "赠送积分权益需填写赠送积分数量（扩展配置 JSON 中设置 giftPoint）");
        }
        Object raw = obj.get("giftPoint");
        if (raw == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "赠送积分权益需在扩展配置中设置 giftPoint（0-" + GIFT_POINT_CONFIG_MAX + "）");
        }
        int giftPoint;
        try {
            if (raw instanceof Number) {
                giftPoint = ((Number) raw).intValue();
            } else {
                giftPoint = Integer.parseInt(raw.toString().trim());
            }
        } catch (NumberFormatException e) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "赠送积分 giftPoint 须为整数");
        }
        if (giftPoint < 0 || giftPoint > GIFT_POINT_CONFIG_MAX) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "赠送积分范围为 0-" + GIFT_POINT_CONFIG_MAX);
        }
    }

    /**
     * 校验券礼包最多配置10种不重复的平台券，每种数量为1～10的整数。
     * 逐项检查couponId、quantity及平台归属，发放时的实际可领取性由领券服务判断。
     *
     * @param benefitConfig 含coupons数组的权益扩展配置JSON
     * @throws ServiceException JSON、列表、平台券关联或赠送数量非法
     */
    private void validateCouponPackageConfig(String benefitConfig) {
        if (CharSequenceUtil.isBlank(benefitConfig)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包权益需配置优惠券（扩展配置 JSON，含 coupons 列表）");
        }
        JSONObject obj;
        try {
            obj = JSONUtil.parseObj(benefitConfig);
        } catch (Exception e) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包权益的扩展配置须为合法 JSON");
        }
        if (obj == null || obj.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包权益需配置 coupons");
        }
        JSONArray coupons = obj.getJSONArray("coupons");
        if (coupons == null || coupons.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包请至少配置一张优惠券（coupons 数组内需包含 couponId、quantity）");
        }
        if (coupons.size() > 10) throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包最多选择10张优惠券");
        Set<String> couponIds = new HashSet<>();
        for (int i = 0; i < coupons.size(); i++) {
            JSONObject item = coupons.getJSONObject(i);
            if (item == null || item.isEmpty()) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包 coupons[" + i + "] 格式不正确");
            }
            String couponId = item.getStr("couponId");
            if (CharSequenceUtil.isBlank(couponId)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包 coupons[" + i + "] 缺少 couponId（平台优惠券 ID）");
            }
            if (!couponIds.add(couponId.trim())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包中同一优惠券不能重复：" + couponId);
            }
            MemberBenefitRules.number(item, "quantity", BigDecimal.ONE, BigDecimal.TEN, 0);
            Coupon configuredCoupon = couponService.getById(couponId.trim());
            if (configuredCoupon == null || !"0".equals(configuredCoupon.getStoreId()))
                throw new ServiceException(ResultCode.PARAMS_ERROR, "请选择有效的平台优惠券");
            Object qtyRaw = item.get("quantity");
            if (qtyRaw == null) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包 coupons[" + i + "] 缺少 quantity（赠送份数 " + COUPON_PKG_QTY_MIN + "-" + COUPON_PKG_QTY_MAX + "）");
            }
            int quantity;
            try {
                if (qtyRaw instanceof Number) {
                    quantity = ((Number) qtyRaw).intValue();
                } else {
                    quantity = Integer.parseInt(qtyRaw.toString().trim());
                }
            } catch (NumberFormatException e) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包 quantity 须为整数");
            }
            if (quantity < COUPON_PKG_QTY_MIN || quantity > COUPON_PKG_QTY_MAX) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "券礼包每张优惠券赠送份数须为 " + COUPON_PKG_QTY_MIN + "-" + COUPON_PKG_QTY_MAX);
            }
        }
    }
}
