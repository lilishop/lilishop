package cn.lili.modules.promotion.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dto.search.MemberCouponSearchParams;
import cn.lili.modules.promotion.entity.vos.MemberCouponVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 客户优惠券业务层
 *
 * @author Chopper
 * @since 2020/11/18 9:45 上午
 */
public interface MemberCouponService extends IService<MemberCoupon> {

    /**
     * 检查该客户领取优惠券的可领取数量
     *
     * @param couponId 优惠券编号
     * @param memberId 客户
     */
    void checkCouponLimit(String couponId, String memberId);

    /**
     * 领取优惠券
     *
     * @param couponId   优惠券编号
     * @param memberId   客户
     * @param memberName 客户名称
     */
    void receiveBuyerCoupon(String couponId, String memberId, String memberName);

    /**
     * 领取直播优惠券
     *
     * @param couponId   优惠券编号
     * @param liveRoomId 直播间ID
     * @param memberId   客户
     * @param memberName 客户名称
     * @return 客户优惠券
     */
    MemberCoupon receiveLiveBuyerCoupon(String couponId, String liveRoomId, String memberId, String memberName);

    /**
     * 领取优惠券
     *
     * @param couponId   优惠券编号
     * @param memberId   客户
     * @param memberName 客户名称
     */
    void receiveCoupon(String couponId, String memberId, String memberName);

    /**
     * 获取客户优惠券列表
     *
     * @param param  查询参数
     * @param pageVo 分页参数
     * @return 客户优惠券列表
     */
    IPage<MemberCoupon> getMemberCoupons(MemberCouponSearchParams param, PageVO pageVo);

    /**
     * 获取客户优惠券列表
     *
     * @param param  查询参数
     * @return 客户优惠券列表
     */
    List<MemberCoupon> getMemberCoupons(MemberCouponSearchParams param);

    /**
     * 获取当前用户的优惠券列表（优先读取缓存）
     *
     * @param memberId 客户id
     * @return 客户优惠券列表
     */
    List<MemberCoupon> getMemberCoupons(String memberId);

    /**
     * 获取客户优惠券列表
     *
     * @param param      查询参数
     * @param totalPrice 当前商品总价
     * @param pageVo     分页参数
     * @return 客户优惠券列表
     */
    IPage<MemberCoupon> getMemberCouponsByCanUse(MemberCouponSearchParams param, Double totalPrice, PageVO pageVo);

    /**
     * 获取当前客户当前商品可用的客户优惠券
     *
     * @param memberId   客户Id
     * @param couponIds  优惠券id列表
     * @param totalPrice 当前商品总价
     * @return 客户优惠券列表
     */
    List<MemberCoupon> getCurrentGoodsCanUse(String memberId, List<String> couponIds, Double totalPrice);

    /**
     * 获取当前客户全品类优惠券
     *
     * @param memberId 客户Id
     * @param storeId  店铺id
     * @return 客户优惠券列表
     */
    List<MemberCoupon> getAllScopeMemberCoupon(String memberId, List<String> storeId);

    /**
     * 获取客户优惠券
     *
     * @param param  查询参数
     * @return 客户优惠券列表
     */
    MemberCoupon getMemberCoupon(MemberCouponSearchParams param);

    /**
     * 获取客户优惠券数量
     *
     * @return 客户优惠券数量
     */
    long getMemberCouponsNum();

    /**
     * 使用优惠券
     *
     * @param ids 客户优惠券id
     */
    void used(String memberId, List<String> ids);

    /**
     * 作废当前客户优惠券
     *
     * @param id id
     */
    void cancellation(String memberId, String id);

    /**
     * 作废无效的客户优惠券
     *
     * @return 是否操作成功
     */
    boolean expireInvalidMemberCoupon(String memberId);

    /**
     * 关闭客户优惠券
     *
     * @param couponIds 优惠券id集合
     */
    void closeMemberCoupon(List<String> couponIds);

    /**
     * 恢复客户优惠券
     *
     * @param memberCouponIds 客户优惠券id列表
     * @return 是否恢复成功
     */
    boolean recoveryMemberCoupon(List<String> memberCouponIds);

    /**
     * 作废优惠券
     *
     * @param couponId 优惠券ID
     */
    void voidCoupon(String couponId);

    /**
     * 获取客户优惠券列表
     *
     * @param page 分页参数
     * @param param 查询参数
     * @return 客户优惠券列表
     */
    Page<MemberCouponVO> getMemberCouponsPage(Page<MemberCoupon> page, MemberCouponSearchParams param);

    /**
     * 获取客户领取过的优惠券数量
     */
    long getMemberCouponNum(String memberId, String couponId);

    /**
     * 按优惠券ID分页查询客户领取记录
     *
     * @param couponId 优惠券ID
     * @param pageVO 分页参数
     * @return 分页结果
     */
    IPage<MemberCoupon> pageByCouponId(String couponId, PageVO pageVO);

}
