package cn.lili.modules.stock.entity.enums;

/**
 * 库存流水变动类型枚举
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public enum StockFlowTypeEnum {

    PROCUREMENT_INBOUND("采购入库"),

    STOCK_OUTBOUND("手工出库"),

    DAMAGE("商品报损"),

    INVENTORY_COUNT("盘点调整"),

    MANUAL_ADJUST("手工调整"),

    ORDER_PAID("订单销售"),

    ORDER_CANCEL("订单取消回滚");

    private final String description;

    StockFlowTypeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
