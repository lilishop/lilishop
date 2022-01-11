package cn.lili.test.promotion;

import cn.hutool.core.util.RandomUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.search.FullDiscountSearchParams;
import cn.lili.modules.promotion.service.FullDiscountService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author paulG
 * @since 2020/10/22
 **/
@ExtendWith(SpringExtension.class)
@SpringBootTest
class FullDiscountTest {

    @Autowired
    private FullDiscountService fullDiscountService;

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Test
    void addFullDiscount() {
        FullDiscountVO fullDiscountVO = new FullDiscountVO();
        fullDiscountVO.setStoreId("131");
        fullDiscountVO.setStoreName("小米自营旗舰店");
        fullDiscountVO.setDescription("full discount test " + RandomUtil.randomNumber());
        fullDiscountVO.setFullMinusFlag(true);
        fullDiscountVO.setFullMoney(130D);
        fullDiscountVO.setFullMinus(100D);
        fullDiscountVO.setFreeFreightFlag(true);

        fullDiscountVO.setPromotionName("FullDiscount-" + fullDiscountVO.getId());
        fullDiscountVO.setTitle("满" + fullDiscountVO.getFullMoney() + "减" + fullDiscountVO.getFullMinus());
        fullDiscountVO.setStartTime(cn.hutool.core.date.DateUtil.parse("2020-11-30 10:35:00"));
        fullDiscountVO.setEndTime(cn.hutool.core.date.DateUtil.parse("2020-12-25 23:20:00"));

        List<PromotionGoods> promotionGoodsLis = new ArrayList<>();
        GoodsSku sku121 = goodsSkuService.getGoodsSkuByIdFromCache("121");
        PromotionGoods promotionGoods = new PromotionGoods(sku121);
        promotionGoods.setPrice(sku121.getPrice());
        promotionGoods.setLimitNum(100);
        promotionGoods.setStartTime(fullDiscountVO.getStartTime());
        promotionGoods.setEndTime(fullDiscountVO.getEndTime());
        promotionGoods.setNum(10);
        promotionGoods.setQuantity(100);
        promotionGoods.setPromotionId(fullDiscountVO.getId());
        promotionGoods.setPromotionType(PromotionTypeEnum.FULL_DISCOUNT.name());
        promotionGoods.setTitle("满" + fullDiscountVO.getFullMoney() + "减" + fullDiscountVO.getFullMinus());
        promotionGoodsLis.add(promotionGoods);
        fullDiscountVO.setPromotionGoodsList(promotionGoodsLis);

        Assertions.assertTrue(fullDiscountService.savePromotions(fullDiscountVO));
    }

    @Test
    void search() {
        PageVO pageVo = new PageVO();
        pageVo.setPageSize(10);
        pageVo.setPageNumber(0);
        pageVo.setNotConvert(true);
        pageVo.setSort("startTime");
        pageVo.setOrder("asc");

        IPage<FullDiscount> fullDiscountByPage = fullDiscountService.pageFindAll(new FullDiscountSearchParams(), null);

        Assertions.assertNotNull(fullDiscountByPage);
    }

    @Test
    void update() {
        FullDiscountVO fullDiscountVO = new FullDiscountVO();
        fullDiscountVO.setId("1325981729404248064");
        fullDiscountVO.setStoreId("132");
        fullDiscountVO.setStoreName("联想自营旗舰店");
        fullDiscountVO.setDescription("Not worth");
        fullDiscountVO.setFullMinusFlag(true);
        fullDiscountVO.setFullMoney(100D);
        fullDiscountVO.setFullMinus(80D);
        fullDiscountVO.setFreeFreightFlag(true);

        fullDiscountVO.setPromotionName("FullDiscount-" + fullDiscountVO.getId());
        fullDiscountVO.setTitle("满" + fullDiscountVO.getFullMoney() + "减" + fullDiscountVO.getFullMinus());
        fullDiscountVO.setStartTime(cn.hutool.core.date.DateUtil.parse("2020-11-10 10:15:00"));
        fullDiscountVO.setEndTime(cn.hutool.core.date.DateUtil.parse("2020-11-10 10:30:00"));

        List<PromotionGoods> promotionGoodsLis = new ArrayList<>();
        PromotionGoods promotionGoods = new PromotionGoods();
        promotionGoods.setSkuId("134");
        promotionGoods.setPrice(18000D);
        promotionGoods.setStartTime(fullDiscountVO.getStartTime());
        promotionGoods.setEndTime(fullDiscountVO.getEndTime());
        promotionGoods.setNum(1);
        promotionGoods.setQuantity(100);
        promotionGoods.setPromotionType(PromotionTypeEnum.FULL_DISCOUNT.name());
        promotionGoods.setTitle("满" + fullDiscountVO.getFullMoney() + "减" + fullDiscountVO.getFullMinus());
        promotionGoods.setLimitNum(100);
        promotionGoods.setPromotionId("200");
        promotionGoods.setStoreId("132");
        promotionGoodsLis.add(promotionGoods);
        fullDiscountVO.setPromotionGoodsList(promotionGoodsLis);
        Assertions.assertTrue(fullDiscountService.updatePromotions(fullDiscountVO));
    }

    @Test
    void delete() {
        Assertions.assertTrue(fullDiscountService.removePromotions(Collections.singletonList("1325995092947525632")));
    }


}
