package cn.lili.modules.order.order.entity.enums;

/**
 * 标识是否被4.2.4版本处理过的订单
 * @author: ftyy
 * @Date: 2021-11-19 18:29
 */
public enum IdentificationStatusEnum {
    /**
     * 订单处理状态
     **/

    NOT_HANDLE("未处理过的订单"),
    ALREADY_NOT_HANDLE("已处理过的订单");

    private final String description;

    IdentificationStatusEnum(String description) {
        this.description = description;
    }

    public String description() {
        return this.description;
    }
}
