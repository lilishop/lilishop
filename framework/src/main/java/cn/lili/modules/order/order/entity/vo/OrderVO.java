package cn.lili.modules.order.order.entity.vo;

import cn.hutool.core.bean.BeanUtil;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * 订单vo
 *
 * @author Bulbasaur
 * @since 2020/11/28 11:38
 */
@Data
@NoArgsConstructor
public class OrderVO extends Order {


    private static final long serialVersionUID = 5820637554656388777L;

    @ApiModelProperty(value = "订单商品项目")
    private List<OrderItem> orderItems;


    public OrderVO (Order order,List<OrderItem> orderItems){
        BeanUtil.copyProperties(order, this);
        this.setOrderItems(orderItems);
    }
}
