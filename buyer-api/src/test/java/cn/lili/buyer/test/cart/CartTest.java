package cn.lili.buyer.test.cart;

import cn.hutool.json.JSONUtil;
import cn.lili.modules.goods.entity.vos.CategoryVO;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.vo.TradeParams;
import cn.lili.modules.order.cart.service.CartService;
import cn.lili.modules.payment.service.PaymentService;
import lombok.extern.slf4j.Slf4j;
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
@Slf4j
@ExtendWith(SpringExtension.class)
@SpringBootTest
class CartTest {

    @Autowired
    private CartService cartService;

    @Autowired
    private CategoryService categoryService;

    @Autowired
    private PaymentService paymentService;


    /**
     * 查询当前会员购物车及商品信息。
     */
    @Test
    void getAll() {
        TradeDTO allTradeDTO = cartService.getAllTradeDTO();
        Assertions.assertNotNull(allTradeDTO);
        log.info("{}", JSONUtil.toJsonStr(allTradeDTO));
    }

    /**
     * 调用购物车删除接口移除指定SKU。
     */
    @Test
    void deleteAll() {
        cartService.delete(new String[]{"1344220459059404800"});
        Assertions.assertTrue(true);
    }

    /**
     * 使用默认交易参数创建交易。
     */
    @Test
    void createTrade() {
//       TradeDTO allTradeDTO = cartService.getAllTradeDTO();
//       Assert.assertNotNull(allTradeDTO);
//       System.out.println(JsonUtil.objectToJson(allTradeDTO));
        cartService.createTrade(new TradeParams());
    }

    /**
     * 查询商品分类数据。
     */
    @Test
    void getAllCategory() {
        List<CategoryVO> allCategory = categoryService.categoryTree();
        for (CategoryVO categoryVO : allCategory) {
            log.info("{}", categoryVO);
        }
        Assertions.assertTrue(true);
    }


    /**
     * 调用购物车接口选用指定优惠券。
     */
    @Test
    void storeCoupon() {
        cartService.selectCoupon("1333318596239843328", CartTypeEnum.CART.name(), true);
        Assertions.assertTrue(true);
    }

}
