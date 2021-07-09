package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.enums.CouponScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.MemberCouponStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.vos.CouponSearchParams;
import cn.lili.modules.promotion.mapper.MemberCouponMapper;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.MemberCouponService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

/**
 * 会员优惠券业务层实现
 *
 * @author Chopper
 * @date 2020/8/21
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class MemberCouponServiceImpl extends ServiceImpl<MemberCouponMapper, MemberCoupon> implements MemberCouponService {

    /**
     * 优惠券
     */
    @Autowired
    private CouponService couponService;

    @Override
    public void checkCouponLimit(String couponId, String memberId) {
        Coupon coupon = couponService.getById(couponId);
        LambdaQueryWrapper<MemberCoupon> queryWrapper = new LambdaQueryWrapper<MemberCoupon>()
                .eq(MemberCoupon::getCouponId, couponId)
                .eq(MemberCoupon::getMemberId, memberId);
        int haveCoupons = this.count(queryWrapper);
        if (!PromotionStatusEnum.START.name().equals(coupon.getPromotionStatus())) {
            throw new ServiceException(ResultCode.COUPON_RECEIVE_ERROR);
        }
        if (coupon.getPublishNum() != 0 && coupon.getReceivedNum() >= coupon.getPublishNum()) {
            throw new ServiceException(ResultCode.COUPON_NUM_INSUFFICIENT_ERROR);
        }
        if (haveCoupons >= coupon.getCouponLimitNum()) {
            throw new ServiceException("此优惠券最多领取" + coupon.getCouponLimitNum() + "张");
        }
    }

    @Override
    public void receiveCoupon(String couponId, String memberId, String memberName) {
        Coupon coupon = couponService.getCouponDetailFromMongo(couponId);
        if (coupon != null) {
            this.checkCouponLimit(couponId, memberId);
            MemberCoupon memberCoupon = new MemberCoupon(coupon);
            memberCoupon.setMemberId(memberId);
            memberCoupon.setMemberName(memberName);
            memberCoupon.setMemberCouponStatus(MemberCouponStatusEnum.NEW.name());
            memberCoupon.setIsPlatform(("platform").equals(coupon.getStoreId()));
            this.save(memberCoupon);
            couponService.receiveCoupon(couponId, 1);
        } else {
            throw new ServiceException(ResultCode.COUPON_NOT_EXIST);
        }
    }

    @Override
    public IPage<MemberCoupon> getMemberCoupons(CouponSearchParams param, PageVO pageVo) {
        QueryWrapper<MemberCoupon> queryWrapper = param.wrapper();
        return this.page(PageUtil.initPage(pageVo), queryWrapper);
    }

    /**
     * 获取会员优惠券列表
     *
     * @param param  查询参数
     * @param pageVo 分页参数
     * @return 会员优惠券列表
     */
    @Override
    public IPage<MemberCoupon> getMemberCouponsByCanUse(CouponSearchParams param, Double totalPrice, PageVO pageVo) {
        LambdaQueryWrapper<MemberCoupon> queryWrapper = new LambdaQueryWrapper<>();
        List<String> storeIds = new ArrayList<>(Arrays.asList(param.getStoreId().split(",")));
        storeIds.add("platform");
        queryWrapper.in(MemberCoupon::getStoreId, storeIds);
        queryWrapper.eq(MemberCoupon::getMemberId, param.getMemberId());
        queryWrapper.and(
                i -> i.like(MemberCoupon::getScopeId, param.getScopeId())
                        .or(j -> j.eq(MemberCoupon::getScopeType, CouponScopeTypeEnum.ALL.name())));
        queryWrapper.eq(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.NEW.name());
        queryWrapper.le(MemberCoupon::getConsumeThreshold, totalPrice);
        queryWrapper.gt(MemberCoupon::getEndTime, new Date());
        return this.page(PageUtil.initPage(pageVo), queryWrapper);
    }

    /**
     * 获取当前会员当前商品可用的会员优惠券
     *
     * @param memberId  会员Id
     * @param couponIds 优惠券id列表
     * @return 会员优惠券列表
     */
    @Override
    public List<MemberCoupon> getCurrentGoodsCanUse(String memberId, List<String> couponIds, Double totalPrice) {
        LambdaQueryWrapper<MemberCoupon> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(MemberCoupon::getMemberId, memberId);
        queryWrapper.eq(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.NEW.name());
        queryWrapper.in(MemberCoupon::getCouponId, couponIds);
        queryWrapper.ne(MemberCoupon::getScopeType, CouponScopeTypeEnum.ALL.name());
        queryWrapper.le(MemberCoupon::getConsumeThreshold, totalPrice);
        queryWrapper.gt(MemberCoupon::getEndTime, new Date());
        return this.list(queryWrapper);
    }

    /**
     * 获取当前会员全品类优惠券
     *
     * @param memberId 会员Id
     * @param storeId  店铺id
     * @return 会员优惠券列表
     */
    @Override
    public List<MemberCoupon> getAllScopeMemberCoupon(String memberId, List<String> storeId) {
        LambdaQueryWrapper<MemberCoupon> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(MemberCoupon::getMemberId, memberId);
        queryWrapper.eq(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.NEW.name());
        queryWrapper.eq(MemberCoupon::getScopeType, CouponScopeTypeEnum.ALL.name());
        queryWrapper.gt(MemberCoupon::getEndTime, new Date()).and(i -> i.in(MemberCoupon::getStoreId, storeId).or(j -> j.eq(MemberCoupon::getIsPlatform, true)));
        return this.list(queryWrapper);
    }

    @Override
    public Integer getMemberCouponsNum() {
        QueryWrapper<MemberCoupon> queryWrapper = Wrappers.query();
        queryWrapper.eq("member_id", UserContext.getCurrentUser().getId());
        queryWrapper.eq("member_coupon_status", MemberCouponStatusEnum.NEW.name());
        queryWrapper.eq("delete_flag", false);
        return this.count(queryWrapper);
    }

    /**
     * 更新会员优惠券状态
     *
     * @param status 要变更的状态
     * @param id     会员优惠券id
     */
    @Override
    public void updateMemberCouponStatus(MemberCouponStatusEnum status, String id) {
        MemberCoupon memberCoupon = this.getById(id);
        if (memberCoupon == null) {
            throw new ServiceException(ResultCode.COUPON_MEMBER_NOT_EXIST);
        }
        String memberCouponStatus = memberCoupon.getMemberCouponStatus();
        if (memberCouponStatus.equals(MemberCouponStatusEnum.NEW.name()) || memberCouponStatus.equals(MemberCouponStatusEnum.USED.name())) {
            LambdaUpdateWrapper<MemberCoupon> updateWrapper = new LambdaUpdateWrapper<>();
            updateWrapper.eq(MemberCoupon::getId, id).set(MemberCoupon::getMemberCouponStatus, status.name());
            this.update(updateWrapper);
        } else {
            throw new ServiceException(ResultCode.COUPON_MEMBER_STATUS_ERROR);
        }
    }


    @Override
    public void used(List<String> ids) {
        if (ids != null && !ids.isEmpty()) {
            List<MemberCoupon> memberCoupons = this.listByIds(ids);

            //如果查出来的优惠券数量不一致
            if (memberCoupons.size() != ids.size()) {
                throw new ServiceException(ResultCode.COUPON_EXPIRED);
            }
            //循环处理
            memberCoupons.forEach(item -> {
                if (!item.getMemberCouponStatus().equals(MemberCouponStatusEnum.NEW.name())) {
                    throw new ServiceException(ResultCode.COUPON_EXPIRED);
                }
                item.setMemberCouponStatus(MemberCouponStatusEnum.USED.name());
                item.setConsumptionTime(new Date());
            });

            this.updateBatchById(memberCoupons);
        }
    }

    /**
     * 作废当前会员优惠券
     *
     * @param id id
     */
    @Override
    public void cancellation(String id) {
        LambdaUpdateWrapper<MemberCoupon> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(MemberCoupon::getId, id);
        updateWrapper.set(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.CLOSED.name());
        this.update(updateWrapper);
    }

}