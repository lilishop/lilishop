package cn.lili.modules.promotion.service;

import cn.lili.modules.promotion.entity.dos.FlashDiscount;
import cn.lili.modules.promotion.entity.vos.FlashDiscountVO;

public interface FlashDiscountService extends AbstractPromotionsService<FlashDiscount> {

    FlashDiscountVO getFlashDiscountVO(String id);
}
