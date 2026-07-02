package cn.lili.modules.promotion.service;

import cn.lili.modules.promotion.entity.dos.NthItemDiscount;
import cn.lili.modules.promotion.entity.vos.NthItemDiscountVO;

public interface NthItemDiscountService extends AbstractPromotionsService<NthItemDiscount> {

    NthItemDiscountVO getNthItemDiscountVO(String id);
}
