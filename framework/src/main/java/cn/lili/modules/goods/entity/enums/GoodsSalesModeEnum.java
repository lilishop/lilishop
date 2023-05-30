package cn.lili.modules.goods.entity.enums;

/**
 * 销售模式
 *
 * @author pikachu
 * @since 2020-02-26 23:24:13
 */
public enum GoodsSalesModeEnum {

    RETAIL("零售"),
    WHOLESALE("批发");

    private final String description;

    GoodsSalesModeEnum(String description) {
        this.description = description;

    }

    public String description() {
        return description;
    }

}
