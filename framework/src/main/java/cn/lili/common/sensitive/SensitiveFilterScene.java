package cn.lili.common.sensitive;

/**
 * 敏感词过滤场景分组枚举
 *
 * @author Bulbasaur
 * @date 2026-10-29
 */
public enum SensitiveFilterScene {

    /**
     * 会员信息（用户昵称、收货地址信息）
     */
    MEMBER("会员信息"),
    /**
     * 产品评价（用户发布评论信息）
     */
    EVALUATION("产品评价"),
    /**
     * 商家（商家入驻信息）
     */
    STORE("商家"),
    /**
     * 产品信息（商品发布、商品修改、商品分类信息）
     */
    GOODS("产品信息"),
    /**
     * 促销活动（活动名称、优惠券名称信息）
     */
    PROMOTION("促销活动"),
    /**
     * 装修模块（导航名称、装修楼层中的标题和子标题）
     */
    DECORATION("装修模块"),
    /**
     * 原因管理（退款/退货/换货原因信息）
     */
    REASON("原因管理"),
    /**
     * 资讯管理（分类名称、文章标题、文章详情）
     */
    ARTICLE("资讯管理");

    private final String description;

    SensitiveFilterScene(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }
}
