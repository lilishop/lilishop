package cn.lili.test.promotion;

import cn.lili.modules.promotion.entity.dos.BasePromotions;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.service.PromotionService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Map;

/**
 * @author paulG
 * @since 2020/11/23
 **/
@ExtendWith(SpringExtension.class)
@SpringBootTest
class PromotionPriceTest {

    @Autowired
    private PromotionService promotionService;

    @Autowired
    private PromotionGoodsService promotionGoodsServiceService;

    @Test
    void testSeckillPrice() {
        Map<String, Object> currentPromotion = promotionService.getCurrentPromotion();
        for (Map.Entry<String, Object> entry : currentPromotion.entrySet()) {
            BasePromotions promotion = (BasePromotions) entry.getValue();
            System.out.println(entry.getKey() + "-" + promotion.getId());
        }
        Assertions.assertTrue(true);
    }

}
