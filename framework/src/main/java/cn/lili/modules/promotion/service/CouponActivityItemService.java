package cn.lili.modules.promotion.service;

import cn.lili.modules.promotion.entity.dos.CouponActivityItem;
import cn.lili.modules.promotion.entity.vos.CouponActivityItemVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 优惠券活动-优惠券业务层
 *
 * @author Bulbasaur
 * @since 2021/5/20 6:10 下午
 */
public interface CouponActivityItemService extends IService<CouponActivityItem> {

    /**
     * 获取优惠券活动关联优惠券列表
     *
     * @param activityId 优惠券活动ID
     * @return 优惠券关联优惠券列表
     */
    List<CouponActivityItem> getCouponActivityList(String activityId);
    /**
     * 获取优惠券活动关联优惠券列表VO
     *
     * @param activityId 优惠券活动ID
     * @return 优惠券关联优惠券列表
     */
    List<CouponActivityItemVO> getCouponActivityItemListVO(String activityId);

    /**
     * 根据优惠券id删除优惠活动关联信息项
     *
     * @param couponIds 优惠券id集合
     */
    void removeByCouponId(List<String> couponIds);
}
