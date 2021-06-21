package cn.lili.test.promotion;

import cn.hutool.core.util.RandomUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.vos.FullDiscountSearchParams;
import cn.lili.modules.promotion.service.FullDiscountService;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.ArrayList;
import java.util.List;

/**
 * @author paulG
 * @since 2020/10/22
 **/
@RunWith(SpringRunner.class)
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
        fullDiscountVO.setNumber(1);
        fullDiscountVO.setDescription("full discount test " + RandomUtil.randomNumber());
        fullDiscountVO.setIsFullMinus(true);
        fullDiscountVO.setFullMoney(130D);
        fullDiscountVO.setFullMinus(100D);
        fullDiscountVO.setPromotionStatus(PromotionStatusEnum.NEW.name());
        fullDiscountVO.setIsFreeFreight(true);

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
        promotionGoods.setPromotionStatus(PromotionStatusEnum.NEW.name());
        promotionGoods.setPromotionType(PromotionTypeEnum.FULL_DISCOUNT.name());
        promotionGoods.setTitle("满" + fullDiscountVO.getFullMoney() + "减" + fullDiscountVO.getFullMinus());
        promotionGoodsLis.add(promotionGoods);
        fullDiscountVO.setPromotionGoodsList(promotionGoodsLis);

        Assertions.assertNotNull(fullDiscountService.addFullDiscount(fullDiscountVO));
    }

    @Test
    void searchFromMongo() {
        PageVO pageVo = new PageVO();
        pageVo.setPageSize(10);
        pageVo.setPageNumber(0);
        pageVo.setNotConvert(true);
        pageVo.setSort("startTime");
        pageVo.setOrder("asc");

        IPage<FullDiscountVO> fullDiscountByPageFromMongo = fullDiscountService.getFullDiscountByPageFromMongo(new FullDiscountSearchParams(), null);

        Assertions.assertNotNull(fullDiscountByPageFromMongo);
        FullDiscount fullDiscount = JSONUtil.toBean(JSONUtil.parseObj(fullDiscountByPageFromMongo.getPages()), FullDiscount.class);
        System.out.println(fullDiscount);
//       fullDiscountByPageFromMongo.forEach(System.out::println);
    }

    @Test
    void update() {
        FullDiscountVO fullDiscountVO = new FullDiscountVO();
        fullDiscountVO.setId("1325981729404248064");
        fullDiscountVO.setStoreId("132");
        fullDiscountVO.setStoreName("联想自营旗舰店");
        fullDiscountVO.setNumber(1);
        fullDiscountVO.setDescription("Not worth");
        fullDiscountVO.setIsFullMinus(true);
        fullDiscountVO.setFullMoney(100D);
        fullDiscountVO.setFullMinus(80D);
        fullDiscountVO.setPromotionStatus(PromotionStatusEnum.NEW.name());
        fullDiscountVO.setIsFreeFreight(true);

        fullDiscountVO.setPromotionName("FullDiscount-" + fullDiscountVO.getId());
        fullDiscountVO.setTitle("满" + fullDiscountVO.getFullMoney() + "减" + fullDiscountVO.getFullMinus());
        fullDiscountVO.setStartTime(cn.hutool.core.date.DateUtil.parse("2020-11-10 10:15:00"));
        fullDiscountVO.setEndTime(cn.hutool.core.date.DateUtil.parse("2020-11-10 10:30:00"));

        List<PromotionGoods> promotionGoodsLis = new ArrayList<>();
        PromotionGoods promotionGoods = new PromotionGoods();
        promotionGoods.setSkuId("134");
        promotionGoods.setPromotionStatus(PromotionStatusEnum.NEW.name());
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
        Assertions.assertNotNull(fullDiscountService.modifyFullDiscount(fullDiscountVO));
    }

    @Test
    void delete() {
        Assertions.assertTrue(fullDiscountService.deleteFullDiscount("1325995092947525632"));
    }


}
