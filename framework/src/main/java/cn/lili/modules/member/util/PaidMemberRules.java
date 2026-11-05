package cn.lili.modules.member.util;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.system.entity.dto.PaidMemberSetting;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.ZoneId;
import java.util.Date;
/**
 * 付费会员日期和金额规则，金额均精确到分。
 * @author Bulbasaur
 * @since 2026-11-04
 */
public final class PaidMemberRules {
    /**
     * 工具类不允许实例化。
     */
    private PaidMemberRules() { }
    /**
     * 按上海时区增加自然月，目标月份无对应日期时取该月末日。
     *
     * @param start 有效期开始时间
     * @param months 自然月数，调用方保证为合法正数
     * @return 有效期结束时间，资格区间不包含该时刻
     */
    public static Date endTime(Date start, int months) {
        return Date.from(start.toInstant().atZone(ZoneId.of("Asia/Shanghai")).plusMonths(months).toInstant());
    }
    /**
     * 会员价按单件四舍五入到分。
     *
     * @param price 原单价，单位元
     * @param discount 折扣，单位折
     * @return 会员单价，单位元
     */
    public static BigDecimal price(BigDecimal price, BigDecimal discount) {
        return price.multiply(discount).divide(BigDecimal.TEN, 2, RoundingMode.HALF_UP);
    }
    /**
     * 按累计退货数量分摊补贴并计算本次差额，各累计分摊金额四舍五入到分。
     * 全部退完时累计冲回金额等于原补贴，避免逐笔独立舍入产生尾差。
     *
     * @param total 原订单项补贴，单位元
     * @param purchased 原购买数量
     * @param previouslyRefunded 此前已退款数量
     * @param quantity 本次退款数量
     * @return 本次冲回补贴，单位元
     * @throws ServiceException 数量非法或累计退款数量超过购买数量
     */
    public static BigDecimal refundSubsidy(BigDecimal total, int purchased, int previouslyRefunded, int quantity) {
        if (purchased <= 0 || previouslyRefunded < 0 || quantity <= 0 || previouslyRefunded + quantity > purchased)
            throw new ServiceException(ResultCode.PARAMS_ERROR, "退款商品数量不正确");
        BigDecimal denominator = BigDecimal.valueOf(purchased);
        return total.multiply(BigDecimal.valueOf(previouslyRefunded + quantity)).divide(denominator, 2, RoundingMode.HALF_UP)
                .subtract(total.multiply(BigDecimal.valueOf(previouslyRefunded)).divide(denominator, 2, RoundingMode.HALF_UP));
    }
    /**
     * 校验折扣范围和有效小数位。
     *
     * @param value 折扣，单位折，大于 0 且不超过 10，最多两位小数
     * @throws ServiceException 折扣为空、越界或精度非法
     */
    public static void discount(BigDecimal value) {
        if (value == null || value.signum() <= 0 || value.compareTo(BigDecimal.TEN) > 0 || value.stripTrailingZeros().scale() > 2)
            throw new ServiceException(ResultCode.PARAMS_ERROR, "折扣必须大于0且不超过10，最多两位小数");
    }
    /**
     * 校验金额、期限、展示、列表规模及自定义范围完整性。
     * 关联对象存在性及折扣商品商家归属由服务层继续检查。
     *
     * @param c 待保存或用于购买的会员配置
     * @throws ServiceException 必填项、范围、精度或配置组合非法
     */
    public static void validate(PaidMemberSetting c) {
        if (c == null || c.getName() == null || c.getName().isBlank() || c.getName().length() > 50)
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请填写50字以内的会员名称");
        if (c.getPrice() == null || c.getPrice().signum() <= 0 || c.getPrice().compareTo(new BigDecimal("1000000")) > 0 || c.getPrice().stripTrailingZeros().scale() > 2)
            throw new ServiceException(ResultCode.PARAMS_ERROR, "开通价格须大于0且不超过1000000元，最多两位小数");
        if (c.getMonths() == null || c.getMonths() < 1 || c.getMonths() > 120)
            throw new ServiceException(ResultCode.PARAMS_ERROR, "有效期为1～120个月");
        if (c.getBackgroundType() == null || !java.util.List.of("COLOR", "IMAGE").contains(c.getBackgroundType()))
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请选择背景色或背景图");
        if ("IMAGE".equals(c.getBackgroundType()) && (c.getBackgroundImage() == null || c.getBackgroundImage().isBlank()))
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请选择会员背景图");
        if (Boolean.TRUE.equals(c.getEnabled()) && (c.getAgreement() == null || c.getAgreement().replaceAll("<[^>]*>", "").isBlank()))
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请配置会员协议后再开放购买");
        if (c.getStoreScope() == null || c.getDiscountMode() == null || !java.util.List.of("SELF", "CUSTOM").contains(c.getStoreScope()) || !java.util.List.of("ALL", "CUSTOM").contains(c.getDiscountMode()))
            throw new ServiceException(ResultCode.PARAMS_ERROR, "商家范围或折扣方式不正确");
        if ("CUSTOM".equals(c.getStoreScope()) && (c.getStoreIds() == null || c.getStoreIds().isEmpty()))
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请选择适用商家");
        discount(c.getDiscount());
        if (c.getGoodsRules() == null || c.getBenefitIds() == null || c.getRecommendedSkuIds() == null || c.getStoreIds() == null)
            throw new ServiceException(ResultCode.PARAMS_ERROR, "配置列表不能为空值");
        java.util.Set<String> ids = new java.util.HashSet<>();
        for (var rule : c.getGoodsRules()) {
            if (rule == null || rule.getSkuId() == null || rule.getSkuId().isBlank() || !ids.add(rule.getSkuId()))
                throw new ServiceException(ResultCode.PARAMS_ERROR, "商品SKU不能为空或重复");
            discount(rule.getDiscount());
        }
        if ("CUSTOM".equals(c.getDiscountMode()) && c.getGoodsRules().isEmpty())
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请选择折扣商品");
        if (c.getRecommendedSkuIds().size() > 200 || c.getGoodsRules().size() > 2000)
            throw new ServiceException(ResultCode.PARAMS_ERROR, "推荐商品最多200个，折扣商品最多2000个");
        if (c.getBenefitIds().size() > 200 || c.getStoreIds().size() > 2000)
            throw new ServiceException(ResultCode.PARAMS_ERROR, "权益最多200项，商家最多2000个");
        for (String text : new String[]{c.getUnopenedText(), c.getOpenedText()})
            if (text == null || text.isBlank() || text.length() > 50) throw new ServiceException(ResultCode.PARAMS_ERROR, "会员入口文案须为1～50字");
        for (String color : new String[]{c.getFontColor(), c.getBackgroundColor(), c.getUnopenedFontColor(), c.getOpenedFontColor()})
            if (color == null || !color.matches("#[0-9a-fA-F]{6}")) throw new ServiceException(ResultCode.PARAMS_ERROR, "颜色须为6位十六进制色值");
    }
}
