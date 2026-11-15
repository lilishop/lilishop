package cn.lili.modules.promotion.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.CouponActivity;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dto.CouponActivityTrigger;
import cn.lili.modules.promotion.entity.vos.CouponActivityVO;
import cn.lili.modules.promotion.entity.vos.CouponPackageQrCodeVO;
import cn.lili.modules.promotion.entity.vos.CouponPackageVO;
import com.baomidou.mybatisplus.core.metadata.IPage;

import java.util.List;

/**
 * 优惠券活动业务层
 *
 * @author Bulbasaur
 * @since 2021/5/20 6:10 下午
 */
public interface CouponActivityService extends AbstractPromotionsService<CouponActivity> {


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
     * @param couponActivity 精准发券
     */
    void specify(CouponActivity couponActivity);

    /**
     * 精准发券
     *
     * @param couponActivityId 精准发券活动ID
     */
    void specifyCoupon(String couponActivityId);

    /**
     * 用户优惠券活动触发
     *
     * @return 优惠券列表
     */
    List<MemberCoupon> trigger(CouponActivityTrigger couponActivityTrigger);

    /**
     * 优惠券活动分页查询
     *
     * @param pageVO 分页参数
     * @param couponActivity 查询参数
     * @return 分页结果
     */
    IPage<CouponActivity> getByPage(PageVO pageVO, CouponActivity couponActivity);

    /**
     * 获取优惠券礼包详情
     * 用于买家端独立活动页面展示
     *
     * @param couponActivityId 优惠券活动ID
     * @return 优惠券礼包VO
     */
    CouponPackageVO getCouponPackageDetail(String couponActivityId);

    /**
     * 买家一键领取优惠券礼包
     * 批量为当前登录买家派发礼包内所有有效优惠券
     *
     * @param couponActivityId 优惠券活动ID
     * @return 成功发放的会员优惠券列表
     */
    List<MemberCoupon> receiveCouponPackage(String couponActivityId);

    /**
     * 获取优惠券礼包H5推广二维码、链接及微信小程序葵花码，微信失败时保留H5结果。
     *
     * @param couponActivityId 优惠券活动ID
     * @param h5BaseUrl H5站点前缀域名（若为空则使用默认配置）
     * @return 二维码VO
     */
    CouponPackageQrCodeVO getCouponPackageQrCode(String couponActivityId, String h5BaseUrl);

}
