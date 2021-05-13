package cn.lili.buyer.test.cart;

import cn.lili.modules.promotion.service.MemberCouponService;
import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * @author paulG
 * @since 2020/11/27
 **/
@RunWith(SpringRunner.class)
@SpringBootTest
class MemberCouponTest {

    @Autowired
    private MemberCouponService memberCouponService;

    @Test
    void receiveCoupon() {
        memberCouponService.receiveCoupon("1333318596239843328", "1326834797335306240", "1");
        Assert.assertTrue(true);
    }


}
