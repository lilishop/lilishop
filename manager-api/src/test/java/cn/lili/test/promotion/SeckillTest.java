package cn.lili.test.promotion;

import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.enums.PromotionsApplyStatusEnum;
import cn.lili.modules.promotion.entity.vos.SeckillApplyVO;
import cn.lili.modules.promotion.service.SeckillApplyService;
import cn.lili.modules.promotion.service.SeckillService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.SeckillSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.google.gson.Gson;
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

    /**
     * 系统设置
     */
    @Autowired
    private SettingService settingService;

    @Test
    void add() {
        Setting setting = settingService.get(SettingEnum.SECKILL_SETTING.name());
        System.out.println(setting);
        SeckillSetting seckillSetting = new Gson().fromJson(setting.getSettingValue(), SeckillSetting.class);
        System.out.println(seckillSetting);
        boolean result = true;
        for (int i = 1; i <= SeckillService.PRE_CREATION; i++) {
            Seckill seckill = new Seckill(i, seckillSetting.getHours(), seckillSetting.getSeckillRule());
            seckillService.savePromotions(seckill);
        }
        Assertions.assertTrue(result);
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
