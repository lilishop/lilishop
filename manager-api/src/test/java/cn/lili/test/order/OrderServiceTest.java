package cn.lili.test.order;

import cn.lili.modules.order.order.service.OrderService;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * @author paulG
 * @since 2020/12/1
 **/
@RunWith(SpringRunner.class)
@SpringBootTest
class OrderServiceTest {

    @Autowired
    private OrderService orderService;


    @Test
    void QueryParam() {
//       OrderSearchParams orderSearchParams = new OrderSearchParams();
//       orderSearchParams.setPageSize(0);
//       orderSearchParams.setPageNumber(10);
//       IPage<OrderSimpleVO> orderVOIPage = orderService.queryByParams(orderSearchParams);
//       Assertions.assertNotNull(orderVOIPage);
//       orderVOIPage.getRecords().forEach(System.out::println);
    }


}
