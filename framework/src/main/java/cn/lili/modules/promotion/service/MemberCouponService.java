package cn.lili.modules.promotion.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dto.search.MemberCouponSearchParams;
import cn.lili.modules.promotion.entity.enums.MemberCouponStatusEnum;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 会员优惠券业务层
 *
 * @author Chopper
 * @since 2020/11/18 9:45 上午
 */
public interface MemberCouponService extends IService<MemberCoupon> {

    /**
     * 检查该会员领取优惠券的可领取数量
     *
     * @param couponId 优惠券编号
     * @param memberId 会员
     */
    void checkCouponLimit(String couponId, String memberId);

    /**
     * 领取优惠券
     *
     * @param couponId   优惠券编号
     * @param memberId   会员
     * @param memberName 会员名称
     */
    void receiveBuyerCoupon(String couponId, String memberId, String memberName);

    /**
     * 领取优惠券
     *
     * @param couponId   优惠券编号
     * @param memberId   会员
     * @param memberName 会员名称
     */
    void receiveCoupon(String couponId, String memberId, String memberName);

    /**
     * 获取会员优惠券列表
     *
     * @param param  查询参数
     * @param pageVo 分页参数
     * @return 会员优惠券列表
     */
    IPage<MemberCoupon> getMemberCoupons(MemberCouponSearchParams param, PageVO pageVo);

    /**
     * 获取会员优惠券列表
     *
     * @param pageVo 分页参数
     * @param param  查询参数
     * @return 会员优惠券列表
     */
    List<MemberCoupon> getMemberCoupons(MemberCouponSearchParams param);

    /**
     * 获取会员优惠券列表
     *
     * @param param      查询参数
     * @param totalPrice 当前商品总价
     * @param pageVo     分页参数
     * @return 会员优惠券列表
     */
    IPage<MemberCoupon> getMemberCouponsByCanUse(MemberCouponSearchParams param, Double totalPrice, PageVO pageVo);

    /**
     * 获取当前会员当前商品可用的会员优惠券
     *
     * @param memberId   会员Id
     * @param couponIds  优惠券id列表
     * @param totalPrice 当前商品总价
     * @return 会员优惠券列表
     */
    List<MemberCoupon> getCurrentGoodsCanUse(String memberId, List<String> couponIds, Double totalPrice);

    /**
     * 获取当前会员全品类优惠券
     *
     * @param memberId 会员Id
     * @param storeId  店铺id
     * @return 会员优惠券列表
     */
    List<MemberCoupon> getAllScopeMemberCoupon(String memberId, List<String> storeId);

    /**
     * 获取会员优惠券数量
     *
     * @return 会员优惠券数量
     */
    long getMemberCouponsNum();

    /**
     * 更新会员优惠券状态
     *
     * @param status 要变更的状态
     * @param id     会员优惠券id
     */
    void updateMemberCouponStatus(MemberCouponStatusEnum status, String id);

    /**
     * 使用优惠券
     *
     * @param ids 会员优惠券id
     */
    void used(List<String> ids);

    /**
     * 作废当前会员优惠券
     *
     * @param id id
     */
    void cancellation(String id);

    /**
     * 关闭会员优惠券
     *
     * @param couponIds 优惠券id集合
     */
    void closeMemberCoupon(List<String> couponIds);

}