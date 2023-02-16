package cn.lili.modules.logistics.entity.dto;

import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.store.entity.dos.StoreLogistics;
import cn.lili.modules.store.entity.dto.StoreDeliverGoodsAddressDTO;
import cn.lili.modules.system.entity.dos.Logistics;
import lombok.Data;

import java.util.List;

/**
 * 电子面单DTO
 *
 * @author Bulbasaur
 * @since 2023-02-16
 */
@Data
public class LabelOrderDTO {

    //订单
    Order order;
    //订单货物
    List<OrderItem> orderItems;
    //物流公司
    Logistics logistics;
    //店铺物流公司配置
    StoreLogistics storeLogistics;
    //店铺发件地址
    StoreDeliverGoodsAddressDTO storeDeliverGoodsAddressDTO;
}
