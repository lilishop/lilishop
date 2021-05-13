package cn.lili.modules.goods.entity.enums;

/**
 * 商品关键字类型
 *
 * @author paulG
 * @since 2020/10/15
 **/
public enum GoodsWordsTypeEnum {

    /**
     * 系统
     */
    SYSTEM("系统"),

    /**
     * 平台
     */
    PLATFORM("平台");

    private final String description;

    GoodsWordsTypeEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }

}
