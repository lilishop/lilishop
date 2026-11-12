package cn.lili.modules.stock.entity.enums;

/**
 * 商家出库单状态枚举
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public enum StockOutboundStatusEnum {

    DRAFT("草稿"),

    AUDITING("待审核"),

    COMPLETED("已完成"),

    REJECTED("已驳回"),

    CANCELLED("已作废");

    private final String description;

    StockOutboundStatusEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
