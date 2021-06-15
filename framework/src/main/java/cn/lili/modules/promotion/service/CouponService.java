package cn.lili.modules.promotion.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.vos.CouponSearchParams;
import cn.lili.modules.promotion.entity.vos.CouponVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 优惠券业务层
 *
 * @author Chopper
 * @date 2020/8/21
 */
public interface CouponService extends IService<Coupon> {

    /**
     * 添加优惠券
     *
     * @param coupon 优惠券
     * @return 是否添加成功
     */
    CouponVO add(CouponVO coupon);

    /**
     * 更新优惠卷
     *
     * @param coupon 优惠卷信息
     * @return 是否更新成功
     */
    CouponVO updateCoupon(CouponVO coupon);

    /**
     * 更新优惠卷状态
     *
     * @param couponId        优惠券编号
     * @param promotionStatus 促销状态
     * @return 更新结果
     */
    boolean updateCouponStatus(List<String> couponId, PromotionStatusEnum promotionStatus);

    /**
     * 删除优惠券
     *
     * @param id 优惠券id
     * @return 是否删除成功
     */
    boolean deleteCoupon(String id);


    /**
     * 根据查询条件从mongo中获取优惠券信息列表
     *
     * @param param 查询参数
     * @param page  分页参数
     * @return 优惠券信息列表
     */
    IPage<CouponVO> getCouponsByPageFromMongo(CouponSearchParams param, PageVO page);

    /**
     * 根据查询条件从mongo中获取优惠券信息列表
     *
     * @param param 查询参数
     * @param page  分页参数
     * @return 优惠券信息列表
     */
    IPage<CouponVO> getCanReceiveCoupons(CouponSearchParams param, PageVO page);

    /**
     * 获取优惠券详情
     *
     * @param id 优惠券id
     * @return 优惠券详情
     */
    CouponVO getCouponDetailFromMongo(String id);

    /**
     * 根据条件获取优惠券列表
     *
     * @param param 条件参数
     * @param page  分页条件
     * @return 可领取优惠券集合
     */
    IPage<Coupon> getCouponsByPage(CouponSearchParams param, PageVO page);

    /**
     * 领取优惠券
     *
     * @param couponId   优惠券id
     * @param receiveNum 领取数量
     */
    void receiveCoupon(String couponId, Integer receiveNum);

    /**
     * 使用优惠券
     *
     * @param couponId 优惠券id
     * @param usedNum  使用数量
     */
    void usedCoupon(String couponId, Integer usedNum);




}