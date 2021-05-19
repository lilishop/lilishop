package cn.lili.test.promotion;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dto.BasePromotion;
import cn.lili.modules.promotion.entity.dto.PromotionGoodsDTO;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.service.PromotionPriceService;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Map;

/**
 * @author paulG
 * @since 2020/11/23
 **/
@RunWith(SpringRunner.class)
@SpringBootTest
class PromotionPriceTest {

    @Autowired
    private PromotionPriceService promotionPriceService;

    @Autowired
    private PromotionService promotionService;

    @Autowired
    private PromotionGoodsService promotionGoodsServiceService;

    @Test
    void testSeckillPrice() {
        Map<String, Object> currentPromotion = promotionService.getCurrentPromotion();
        for (Map.Entry<String, Object> entry : currentPromotion.entrySet()) {
            BasePromotion promotion = (BasePromotion) entry.getValue();
            System.out.println(entry.getKey() + "-" + promotion.getId());
        }
        Assertions.assertTrue(true);
    }

    @Test
    void testSeckillPrice1() {
        IPage<PromotionGoodsDTO> promotionGoods = promotionGoodsServiceService.getCurrentPromotionGoods(PromotionTypeEnum.FULL_DISCOUNT.name(), new PageVO());

        ResultMessage<IPage<PromotionGoodsDTO>> data = ResultUtil.data(promotionGoods);
        String s = JSONUtil.toJsonStr(data);
        System.out.println(s);
        Assertions.assertTrue(true);
    }

}
