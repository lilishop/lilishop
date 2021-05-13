package cn.lili.modules.order.order.entity.vo;

import cn.lili.modules.order.order.entity.dos.AfterSale;
import jdk.nashorn.internal.objects.annotations.Getter;

/**
 * 售后VO
 *
 * @author Chopper
 * @date 2021/3/12 10:32 上午
 */
public class AfterSaleVO extends AfterSale {
    /**
     * 初始化自身状态
     */
    @Getter
    public AfterSaleAllowOperation getAfterSaleAllowOperationVO() {

        //设置订单的可操作状态
        return new AfterSaleAllowOperation(this);
    }
}
