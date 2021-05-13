package cn.lili.modules.promotion.service;

import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dto.PromotionPriceDTO;
import cn.lili.modules.promotion.entity.dto.PromotionPriceParamDTO;

import java.util.List;

/**
 * 促销计算
 *
 * @author paulG
 * @date 2020/8/21
 **/
public interface PromotionPriceService {

    /**
     * 计算商品当前所参与的促销活动的价格
     *
     * @param tradeSkuList     促销计算参数
     * @param memberCouponList 使用的优惠券
     * @return 促销计算结果
     */
    PromotionPriceDTO calculationPromotionPrice(List<PromotionPriceParamDTO> tradeSkuList, List<MemberCoupon> memberCouponList);

}
