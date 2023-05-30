package cn.lili.buyer.test.cart;

import cn.hutool.json.JSONUtil;
import cn.lili.modules.goods.entity.vos.CategoryVO;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.vo.TradeParams;
import cn.lili.modules.order.cart.service.CartService;
import cn.lili.modules.payment.service.PaymentService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.List;

/**
 * @author paulG
 * @since 2020/11/14
 **/
@ExtendWith(SpringExtension.class)
@SpringBootTest
class CartTest {

    @Autowired
    private CartService cartService;

    @Autowired
    private CategoryService categoryService;

    @Autowired
    private PaymentService paymentService;


    @Test
    void getAll() {
        TradeDTO allTradeDTO = cartService.getAllTradeDTO();
        Assertions.assertNotNull(allTradeDTO);
        System.out.println(JSONUtil.toJsonStr(allTradeDTO));
    }

    @Test
    void deleteAll() {
        cartService.delete(new String[]{"1344220459059404800"});
        Assertions.assertTrue(true);
    }

    @Test
    void createTrade() {
//       TradeDTO allTradeDTO = cartService.getAllTradeDTO();
//       Assert.assertNotNull(allTradeDTO);
//       System.out.println(JsonUtil.objectToJson(allTradeDTO));
        cartService.createTrade(new TradeParams());
    }

    @Test
    void getAllCategory() {
        List<CategoryVO> allCategory = categoryService.categoryTree();
        for (CategoryVO categoryVO : allCategory) {
            System.out.println(categoryVO);
        }
        Assertions.assertTrue(true);
    }


    @Test
    void storeCoupon() {
        cartService.selectCoupon("1333318596239843328", CartTypeEnum.CART.name(), true);
        Assertions.assertTrue(true);
    }

}
