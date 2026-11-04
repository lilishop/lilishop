package cn.lili.test.order;

import cn.lili.modules.order.order.service.OrderService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

/**
 * @author paulG
 * @since 2020/12/1
 **/
@ExtendWith(SpringExtension.class)
@SpringBootTest
class OrderServiceTest {

    @Autowired
    private OrderService orderService;


    /**
     * 保留订单分页查询示例，当前方法体中的查询代码已注释。
     */
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
