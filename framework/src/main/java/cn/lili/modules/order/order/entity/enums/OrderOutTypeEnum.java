package cn.lili.modules.order.order.entity.enums;

import java.util.ArrayList;
import java.util.List;

/**
 * 订单出库的类型枚举
 *
 * @author Chopper
 * @since 2020/11/17 7:27 下午
 */
public enum OrderOutTypeEnum {

    /**
     * 出库类型枚举
     */
    GOODS("商品"),
    SECKILL_GOODS("秒杀活动商品");


    private final String description;

    OrderOutTypeEnum(String description) {
        this.description = description;
    }

    public String description() {
        return this.description;
    }


    public static List<String> getAll() {
        List<String> all = new ArrayList<>();
        all.add(OrderOutTypeEnum.GOODS.name());
        all.add(OrderOutTypeEnum.SECKILL_GOODS.name());
        return all;
    }
}
