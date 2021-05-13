package cn.lili.modules.store.entity.enums;

/**
 * 结算单状态
 *
 * @author Chopper
 * @date 2020/11/17 4:27 下午
 */
public enum BillStatusEnum {

    OUT("已出账"),
    CHECK("已核对"),
    COMPLETE("已完成");
    private final String description;

    BillStatusEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }

}
