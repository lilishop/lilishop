package cn.lili.rocketmq.tags;

/**
 * @author paulG
 * @since 2020/12/9
 **/
public enum GoodsTagsEnum {

    /**
     * "生成商品索引"
     */
    GENERATOR_GOODS_INDEX("生成商品索引"),
    /**
     * "更新商品索引"
     */
    UPDATE_GOODS_INDEX("更新商品索引"),
    /**
     * "更新商品索引促销信息"
     */
    UPDATE_GOODS_INDEX_PROMOTIONS("更新商品索引促销信息"),
    /**
     * "更新商品索引促销信息"
     */
    DELETE_GOODS_INDEX_PROMOTIONS("更新商品索引促销信息"),

    /**
     * "更新商品索引部分字段"
     */
    UPDATE_GOODS_INDEX_FIELD("更新商品索引部分字段"),
    /**
     * "重置商品索引"
     */
    RESET_GOODS_INDEX("重置商品索引"),
    /**
     * "删除商品"
     */
    GOODS_DELETE("删除商品"),
    /**
     * "审核商品"
     */
    GOODS_AUDIT("审核商品"),
    /**
     * "收藏商品"
     */
    GOODS_COLLECTION("收藏商品"),
    /**
     * "购买商品完成"
     */
    BUY_GOODS_COMPLETE("购买商品完成"),
    /**
     * "删除商品SKU"
     */
    SKU_DELETE("删除商品SKU"),
    /**
     * "查看商品"
     */
    VIEW_GOODS("查看商品"),
    /**
     * "商品评价"
     */
    GOODS_COMMENT_COMPLETE("商品评价");

    private final String description;

    GoodsTagsEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }


}
