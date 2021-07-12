package cn.lili.modules.system.entity.plugin.InstantDelivery.dada.enums;

import lombok.Getter;
import lombok.Setter;

/**
 * 达达配送订单状态
 *
 * @author pikachu
 */
public enum DadaOrderStatusEnum {
    //待接单
    WAIT_RECEIVING(1, "待接单"),
    //待取货
    WAIT_PICK_UP(2, "待取货"),
    //配送中
    DELIVERY_IN_PROGRESS(3, "配送中"),
    //已完成
    COMPLETED(4, "已完成"),
    //已取消
    CANCELLED(5, "已取消"),
    //派单中
    DISTRIBUTION_LEAFLETS(8, "派单中"),
    //妥投异常之物品返回中
    ABNORMAL_BACK(9, "妥投异常之物品返回中"),
    //妥投异常之物品返回完成
    ABNORMAL_COMPLETED(10, "妥投异常之物品返回完成"),
    //骑士到店
    TO_IN_STORE(100, "骑士到店");

    /**
     * 状态
     */
    @Setter
    @Getter
    private Integer status;
    /**
     * 状态文本
     */
    @Setter
    @Getter
    private String text;


    DadaOrderStatusEnum(Integer status, String text) {
        this.status = status;
        this.text = text;
    }

    /**
     * 获取配送模版
     * @param status 状态
     * @return 配送模板
     */
    public static String getText(Integer status) {
        //如果空，则直接返回
        if (status == null) {
            return null;
        }
        //对状态枚举值进行处理
        for (DadaOrderStatusEnum dadaOrderStatusEnum : DadaOrderStatusEnum.values()) {
            if (status.equals(dadaOrderStatusEnum.getStatus())) {
                return dadaOrderStatusEnum.getText();
            }
        }
        return WAIT_RECEIVING.getText();
    }
}
