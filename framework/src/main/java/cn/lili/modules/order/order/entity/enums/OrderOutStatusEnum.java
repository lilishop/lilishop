package cn.lili.modules.order.order.entity.enums;

/**
 * 订单出库状态枚举
 *
 * @author Chopper
 * @since 2020/11/17 7:26 下午
 */
public enum OrderOutStatusEnum {

    /** 等待出库 */
    WAIT("等待出库"),

    /** 出库成功 */
    SUCCESS("出库成功"),

    /** 出库失败 */
    FAIL("出库失败");

    private final String description;

    OrderOutStatusEnum(String description){
        this.description = description;
    }

    public String description(){
        return this.description;
    }



}
