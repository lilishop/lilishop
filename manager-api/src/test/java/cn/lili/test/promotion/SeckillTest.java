package cn.lili.test.promotion;

import cn.hutool.core.date.DateUtil;
import cn.lili.modules.promotion.entity.enums.PromotionApplyStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.enums.SeckillApplyStatusEnum;
import cn.lili.modules.promotion.entity.vos.SeckillApplyVO;
import cn.lili.modules.promotion.entity.vos.SeckillVO;
import cn.lili.modules.promotion.service.SeckillApplyService;
import cn.lili.modules.promotion.service.SeckillService;
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
 * @since 2020/10/29
 **/
@RunWith(SpringRunner.class)
@SpringBootTest
class SeckillTest {

    @Autowired
    private SeckillService seckillService;

    @Autowired
    private SeckillApplyService seckillApplyService;

    @Test
    void add() {
        SeckillVO seckillVO = new SeckillVO();
        seckillVO.setId("123456");
        seckillVO.setStoreIds("132");
        seckillVO.setSeckillApplyStatus(SeckillApplyStatusEnum.NOT_APPLY.name());
        seckillVO.setPromotionStatus(PromotionStatusEnum.NEW.name());
        seckillVO.setApplyEndTime(DateUtil.parse("2020-11-13 23:50:00"));
        seckillVO.setStartTime(DateUtil.parse("2020-11-14 12:00:00"));
        seckillVO.setEndTime(DateUtil.parse("2020-11-14 18:00:00"));
        seckillVO.setHours("13,14,15,16,17");
        seckillVO.setPromotionName("Seckill" + seckillVO.getId());
        seckillVO.setSeckillRule("rule" + seckillVO.getId());
        seckillVO.setStoreId("0");
        seckillVO.setStoreName("platform");
        Assertions.assertTrue(seckillService.saveSeckill(seckillVO));
    }

    @Test
    void addApply() {
        List<SeckillApplyVO> seckillApplyVOS = new ArrayList<>();
        SeckillApplyVO seckillApplyVO = new SeckillApplyVO();
        seckillApplyVO.setGoodsName("Apple MacBook Pro 13.3 新款八核M1芯片 8G 256G SSD 深空灰 笔记本电脑 轻薄本 MYD82CH/A");
        seckillApplyVO.setSkuId("50111");
        seckillApplyVO.setOriginalPrice(20000D);
        seckillApplyVO.setPrice(19000D);
        seckillApplyVO.setPromotionApplyStatus(PromotionApplyStatusEnum.APPLY.name());
        seckillApplyVO.setQuantity(100);
        seckillApplyVO.setSalesNum(0);
        seckillApplyVO.setSeckillId("123456");
        seckillApplyVO.setStoreId("501");
        seckillApplyVO.setStoreName("Apple产品自营旗舰店");
        seckillApplyVO.setTimeLine(17);
        seckillApplyVOS.add(seckillApplyVO);
        seckillApplyVO = new SeckillApplyVO();
        seckillApplyVO.setGoodsName("RedmiBook 16 锐龙版 超轻薄全面屏(6核R5-4500U 16G 512G 100% sRGB高色域)灰 手提 笔记本电脑 小米 红米");
        seckillApplyVO.setSkuId("141");
        seckillApplyVO.setOriginalPrice(10000D);
        seckillApplyVO.setPrice(9000D);
        seckillApplyVO.setPromotionApplyStatus(PromotionApplyStatusEnum.APPLY.name());
        seckillApplyVO.setQuantity(100);
        seckillApplyVO.setSalesNum(0);
        seckillApplyVO.setSeckillId("123456");
        seckillApplyVO.setStoreId("131");
        seckillApplyVO.setStoreName("小米自营旗舰店");
        seckillApplyVO.setTimeLine(16);
        seckillApplyVOS.add(seckillApplyVO);
        seckillApplyService.addSeckillApply("123456", "501", seckillApplyVOS);
        Assertions.assertTrue(true);
    }

}
