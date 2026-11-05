package cn.lili.modules.system.entity.dto;

import lombok.Data;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * 单一付费会员设置，以 PAID_MEMBER_SETTING 为 key 存储于 li_setting.setting_value。
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Data
public class PaidMemberSetting {
    /**
     * 是否开放购买和续费，默认关闭；不影响已支付订单的有效资格。
     */
    private Boolean enabled = false;
    /**
     * 单一付费会员名称，必填且最多 50 字。
     */
    private String name = "付费会员";
    /**
     * 付费会员图标图片地址。
     */
    private String icon;
    /**
     * 会员卡背景类型：COLOR 背景色、IMAGE 背景图。
     */
    private String backgroundType = "COLOR";
    /**
     * 会员卡背景色，格式为 #RRGGBB。
     */
    private String backgroundColor = "#51443B";
    /**
     * 会员卡背景图地址，IMAGE 类型必填。
     */
    private String backgroundImage;
    /**
     * 会员卡字体颜色，格式为 #RRGGBB。
     */
    private String fontColor = "#FFF1D8";
    /**
     * 单次开通或续费价格，单位元，大于 0 且最多两位小数，上限 1000000 元。
     */
    private BigDecimal price = new BigDecimal("99.00");
    /**
     * 每次购买增加的自然月数，范围 1～120；未到期续费从原有效期末尾开始。
     */
    private Integer months = 12;
    /**
     * 关联 li_member_benefit 的权益 ID 列表，最多 200 项，随购买配置保存快照。
     */
    private List<String> benefitIds = new ArrayList<>();
    /**
     * 优惠商家范围：SELF 自营商家、CUSTOM 指定商家。
     */
    private String storeScope = "SELF";
    /**
     * 指定 li_store 主键列表，CUSTOM 范围必填，最多 2000 个。
     */
    private List<String> storeIds = new ArrayList<>();
    /**
     * 商品折扣方式：ALL 统一折扣、CUSTOM 逐 SKU 配置。
     */
    private String discountMode = "ALL";
    /**
     * 统一折扣，单位折，大于 0 且不超过 10，最多两位小数，例如 9.5 表示九五折。
     */
    private BigDecimal discount = new BigDecimal("9.5");
    /**
     * 逐 SKU 折扣规则，CUSTOM 方式必填，最多 2000 项，SKU 不得重复。
     */
    private List<GoodsRule> goodsRules = new ArrayList<>();
    /**
     * 会员中心推荐 li_goods_sku 主键列表，最多 200 个；推荐展示不自动赋予折扣资格。
     */
    private List<String> recommendedSkuIds = new ArrayList<>();
    /**
     * 个人中心未开通或已到期时的入口文案，1～50 字。
     */
    private String unopenedText = "开通付费会员，享专属权益";
    /**
     * 未开通入口字体颜色，格式为 #RRGGBB。
     */
    private String unopenedFontColor = "#F4DFB9";
    /**
     * 未开通入口背景图地址，与会员卡背景图分别配置。
     */
    private String unopenedBackground;
    /**
     * 个人中心有效会员的入口文案，1～50 字。
     */
    private String openedText = "我的付费会员";
    /**
     * 已开通入口字体颜色，格式为 #RRGGBB。
     */
    private String openedFontColor = "#F4DFB9";
    /**
     * 已开通入口背景图地址。
     */
    private String openedBackground;
    /**
     * 商品会员价图片标签地址；报价使用当前展示配置，不固化为历史折扣快照。
     */
    private String priceLabel;
    /**
     * 购买说明富文本，保存时按权益说明规则清理 HTML。
     */
    private String description = "";
    /**
     * 会员协议富文本，开放购买时正文不能为空；随购买配置保存快照。
     */
    private String agreement = "";

    /**
     * 自定义SKU折扣，优惠金额由平台承担。
     * @author Bulbasaur
     * @since 2026-11-04
     */
    @Data
    public static class GoodsRule {
        /**
         * 适用商品的 li_goods_sku 主键，必须属于参与会员优惠的商家。
         */
        private String skuId;
        /**
         * 该 SKU 折扣，单位折，大于 0 且不超过 10，最多两位小数；优惠由平台承担。
         */
        private BigDecimal discount;
    }
}
