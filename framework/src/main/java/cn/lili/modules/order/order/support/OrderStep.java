package cn.lili.modules.order.order.support;


import cn.lili.modules.order.order.entity.enums.OrderOperateEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;

import java.util.ArrayList;
import java.util.List;

/**
 * 订单流程
 *
 * @author Chopper
 * @date 2020/11/17 7:40 下午
 */
public class OrderStep implements Cloneable {


    /**
     * 允许的操作
     */
    private List<OrderOperateEnum> allowableOperate;

    public OrderStep(OrderStatusEnum orderStatus, OrderOperateEnum... operates) {
        /**
         * 订单状态
         */
        this.allowableOperate = new ArrayList<OrderOperateEnum>();
        for (OrderOperateEnum orderOperate : operates) {
            allowableOperate.add(orderOperate);
        }
    }

    @Override
    public Object clone() {
        OrderStep orderStep = null;
        try {
            orderStep = (OrderStep) super.clone();
        } catch (CloneNotSupportedException e) {
            e.printStackTrace();
        }
        ArrayList list = (ArrayList) allowableOperate;
        orderStep.allowableOperate = (List) list.clone();
        return orderStep;
    }

    public List<OrderOperateEnum> getOperate() {
        return this.allowableOperate;
    }

    public static void main(String[] args) throws CloneNotSupportedException {
        OrderStep step1 = new OrderStep(OrderStatusEnum.UNPAID, OrderOperateEnum.CONFIRM, OrderOperateEnum.CANCEL);
        OrderStep step2 = (OrderStep) step1.clone();

        step2.getOperate().remove(OrderOperateEnum.CONFIRM);
        step2.getOperate().add(OrderOperateEnum.PAY);

        System.out.println(step1);
        System.out.println(step2);

        step1.getOperate().forEach(orderOperateEnum -> {
            System.out.println(orderOperateEnum);
        });

        System.out.println("--------");
        step2.getOperate().forEach(orderOperateEnum -> {
            System.out.println(orderOperateEnum);
        });

    }


    public boolean checkAllowable(OrderOperateEnum operate) {
        for (OrderOperateEnum orderOperate : allowableOperate) {
            if (operate.compareTo(orderOperate) == 0) {
                return true;
            }
        }
        return false;
    }

}
