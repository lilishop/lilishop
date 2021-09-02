package cn.lili.buyer.test.cart;

import cn.lili.modules.promotion.service.MemberCouponService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * @author paulG
 * @since 2020/11/27
 **/
@ExtendWith(SpringExtension.class)
@SpringBootTest
class MemberCouponTest {

    @Autowired
    private MemberCouponService memberCouponService;

    @Test
    void receiveCoupon() {
        memberCouponService.receiveCoupon("1333318596239843328", "1326834797335306240", "1");
        assertTrue(true);
    }


}
