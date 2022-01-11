package cn.lili.test.promotion;

import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.promotion.entity.enums.PromotionsApplyStatusEnum;
import cn.lili.modules.promotion.entity.vos.SeckillApplyVO;
import cn.lili.modules.promotion.service.SeckillApplyService;
import cn.lili.modules.promotion.service.SeckillService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.ArrayList;
import java.util.List;

/**
 * @author paulG
 * @since 2020/10/29
 **/
@ExtendWith(SpringExtension.class)
@SpringBootTest
class SeckillTest {

    @Autowired
    private SeckillService seckillService;

    @Autowired
    private SeckillApplyService seckillApplyService;

    @Autowired
    private MemberService memberService;

    @Test
    void add() {
//        SeckillVO seckillVO = new SeckillVO();
//        seckillVO.setId("10000");
//        seckillVO.setStoreIds("132");
//        seckillVO.setSeckillApplyStatus(SeckillApplyStatusEnum.NOT_APPLY.name());
//        seckillVO.setPromotionStatus(PromotionStatusEnum.NEW.name());
//        seckillVO.setApplyEndTime(DateUtil.parse("2021-09-06 14:20:00"));
//        seckillVO.setStartTime(DateUtil.parse("2021-09-06 14:22:00"));
//        seckillVO.setEndTime(DateUtil.parse("2021-09-06 23:59:00"));
//        seckillVO.setHours("15,17,19");
//        seckillVO.setPromotionName("Seckill" + seckillVO.getId());
//        seckillVO.setSeckillRule("rule" + seckillVO.getId());
//        seckillVO.setStoreId("1376433565247471616");
//        seckillVO.setStoreName("platform");
//
//        Assertions.assertTrue(seckillService.saveSeckill(seckillVO));
//        memberService.getUserInfo()
    }

    @Test
    void addApply() {
        List<SeckillApplyVO> seckillApplyVOS = new ArrayList<>();
        SeckillApplyVO seckillApplyVO = new SeckillApplyVO();
        seckillApplyVO.setGoodsName("Apple iPhone 12");
        seckillApplyVO.setSkuId("1387977574860193792");
        seckillApplyVO.setOriginalPrice(4000D);
        seckillApplyVO.setPrice(3600D);
        seckillApplyVO.setPromotionApplyStatus(PromotionsApplyStatusEnum.APPLY.name());
        seckillApplyVO.setQuantity(1);
        seckillApplyVO.setSalesNum(0);
        seckillApplyVO.setSeckillId("10000");
        seckillApplyVO.setStoreId("1376369067769724928");
        seckillApplyVO.setStoreName("Lilishop自营店");
        seckillApplyVO.setTimeLine(15);
        seckillApplyVOS.add(seckillApplyVO);
        seckillApplyVO = new SeckillApplyVO();
        seckillApplyVO.setGoodsName("Apple iPhone 12");
        seckillApplyVO.setSkuId("1387977574864388096");
        seckillApplyVO.setOriginalPrice(4000D);
        seckillApplyVO.setPrice(3600D);
        seckillApplyVO.setPromotionApplyStatus(PromotionsApplyStatusEnum.APPLY.name());
        seckillApplyVO.setQuantity(1);
        seckillApplyVO.setSalesNum(0);
        seckillApplyVO.setSeckillId("10000");
        seckillApplyVO.setStoreId("1376369067769724928");
        seckillApplyVO.setStoreName("Lilishop自营店");
        seckillApplyVO.setTimeLine(15);
        seckillApplyVOS.add(seckillApplyVO);
        seckillApplyService.addSeckillApply("10000", "1376369067769724928", seckillApplyVOS);
        Assertions.assertTrue(true);
    }

}
