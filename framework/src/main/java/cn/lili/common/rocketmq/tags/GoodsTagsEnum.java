package cn.lili.common.rocketmq.tags;

/**
 * @author paulG
 * @since 2020/12/9
 **/
public enum GoodsTagsEnum {

    GENERATOR_GOODS_INDEX("生成商品索引"),
    GOODS_DELETE("删除商品"),
    GOODS_AUDIT("审核商品"),
    GOODS_COLLECTION("收藏商品"),
    BUY_GOODS_COMPLETE("购买商品完成"),
    SKU_DELETE("删除商品SKU"),
    VIEW_GOODS("查看商品"),
    GOODS_COMMENT_COMPLETE("商品评价");

    private final String description;

    GoodsTagsEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }


}
