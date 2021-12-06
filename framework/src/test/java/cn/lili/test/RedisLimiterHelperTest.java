package cn.lili.test;

import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.statistics.serviceimpl.OrderStatisticsServiceImpl;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.ArrayList;
import java.util.List;

/**
 * RedisLimiterHelperTest
 *
 * @author Chopper
 * @version v1.0
 * @since 2020-06-13 12:17
 */

@ExtendWith(SpringExtension.class)
@SpringBootTest
public class RedisLimiterHelperTest {

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    private OrderStatisticsServiceImpl orderStatisticsDataService;


    @Test
    public void orderTest() {



    }


    @Test
    public void testBatchUpdate() {
        OrderItem orderItem = new OrderItem();
        orderItem.setId("1356539557729796097");
        orderItem.setCreateBy("1356539557729796097");


        OrderItem orderItem1 = new OrderItem();
        orderItem1.setId("1356787800921341953");
        orderItem1.setCreateBy("1356787800921341953");


        List<OrderItem> orderItemList = new ArrayList<>();
        orderItemList.add(orderItem);
        orderItemList.add(orderItem1);

        orderItemService.updateBatchById(orderItemList);

    }
}
