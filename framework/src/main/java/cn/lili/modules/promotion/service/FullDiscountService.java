package cn.lili.modules.promotion.service;

import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.promotion.entity.dos.FullDiscount;

import java.util.List;

/**
 * 满优惠业务层
 *
 * @author Chopper
 * @since 2020/8/21
 */
public interface FullDiscountService extends AbstractPromotionsService<FullDiscount> {

    /**
     * 当前满优惠活动
     *
     * @param storeId 商家编号
     * @return 满优惠活动信息
     */
    List<FullDiscountVO> currentPromotion(List<String> storeId);

    /**
     * 获取满优惠活动详情
     *
     * @param id 满优惠KID
     * @return 满优惠活动详情
     */
    FullDiscountVO getFullDiscount(String id);



}