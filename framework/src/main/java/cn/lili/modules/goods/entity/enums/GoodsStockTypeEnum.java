package cn.lili.modules.goods.entity.enums;

/**
 * 库存操作类型
 */
public enum GoodsStockTypeEnum {


    SUB("减"),

    ADD("增");

    private final String description;

    GoodsStockTypeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
    // 根据描述获取枚举实例
    public static GoodsStockTypeEnum fromDescription(String description) {
        for (GoodsStockTypeEnum type : GoodsStockTypeEnum.values()) {
            if (type.getDescription().equals(description)) {
                return type;
            }
        }
        throw new IllegalArgumentException("No matching enum constant for description: " + description);
    }

}
