package cn.lili.modules.stock.entity.enums;

/**
 * 库存流水变动方向枚举
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public enum StockFlowDirectionEnum {

    IN("入库"),

    OUT("出库");

    private final String description;

    StockFlowDirectionEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
