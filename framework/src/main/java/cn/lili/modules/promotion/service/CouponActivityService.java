package cn.lili.modules.promotion.service;

import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.promotion.entity.dos.CouponActivity;
import cn.lili.modules.promotion.entity.dto.CouponActivityDTO;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.vos.CouponActivityVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 优惠券活动业务层
 *
 * @author Bulbasaur
 * @date: 2021/5/20 6:10 下午
 */
public interface CouponActivityService extends IService<CouponActivity> {

    /**
     * 创建优惠券活动--精准发券、新人赠券
     *
     * @param couponActivityDTO 优惠券活动DTO
     * @return 优惠券活动DTO
     */
    CouponActivityDTO addCouponActivity(CouponActivityDTO couponActivityDTO);

    /**
     * 修改优惠券活动--精准发券、新人赠券
     *
     * @param couponActivityDTO 优惠券活动DTO
     * @return 优惠券活动DTO
     */
    CouponActivityDTO updateCouponActivity(CouponActivityDTO couponActivityDTO);

    /**
     * 获取优惠券活动VO
     * 包含优惠券活动信息以及优惠券关联优惠券列表
     *
     * @param couponActivityId 优惠券活动ID
     * @return 优惠券VO
     */
    CouponActivityVO getCouponActivityVO(String couponActivityId);

    /**
     * 精准发券
     *
     * @param couponActivityId 优惠券活动ID
     */
    void specify(String couponActivityId);

    /**
     * 注册赠券
     *
     * @param couponActivityList 优惠券活动
     * @param member             会员
     */
    void registered(List<CouponActivity> couponActivityList, Member member);


    /**
     * 修改优惠券活动状态
     *
     * @param id              活动ID
     * @param promotionStatus 活动状态
     * @return 操作状态
     */
    boolean updateCouponActivityStatus(String id, PromotionStatusEnum promotionStatus);
}
