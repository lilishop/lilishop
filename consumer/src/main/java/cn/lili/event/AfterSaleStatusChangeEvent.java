package cn.lili.event;


import cn.lili.modules.order.order.entity.dos.AfterSale;

/**
 * 售后单改变状态
 *
 * @author Chopper
 * @date 2020/11/17 7:13 下午
 */
public interface AfterSaleStatusChangeEvent {

    /**
     * 售后单改变
     *
     * @param afterSale 售后
     */
    void afterSaleStatusChange(AfterSale afterSale);
}
