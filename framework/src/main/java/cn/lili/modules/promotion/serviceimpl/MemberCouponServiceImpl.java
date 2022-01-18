package cn.lili.modules.promotion.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dto.search.MemberCouponSearchParams;
import cn.lili.modules.promotion.entity.enums.CouponGetEnum;
import cn.lili.modules.promotion.entity.enums.MemberCouponStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.mapper.MemberCouponMapper;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * 会员优惠券业务层实现
 *
 * @author Chopper
 * @since 2020/8/21
 */
@Service
@CacheConfig(cacheNames = "{MemberCoupon}")
public class MemberCouponServiceImpl extends ServiceImpl<MemberCouponMapper, MemberCoupon> implements MemberCouponService {

    /**
     * 优惠券
     */
    @Autowired
    private CouponService couponService;

    /**
     * 缓存
     */
    @Autowired
    private Cache cache;

    @Override
    public void checkCouponLimit(String couponId, String memberId) {
        Coupon coupon = couponService.getById(couponId);
        LambdaQueryWrapper<MemberCoupon> queryWrapper = new LambdaQueryWrapper<MemberCoupon>()
                .eq(MemberCoupon::getCouponId, couponId)
                .eq(MemberCoupon::getMemberId, memberId);
        long haveCoupons = this.count(queryWrapper);
        if (!PromotionsStatusEnum.START.name().equals(coupon.getPromotionStatus())) {
            throw new ServiceException(ResultCode.COUPON_RECEIVE_ERROR);
        }
        if (coupon.getPublishNum() != 0 && coupon.getReceivedNum() >= coupon.getPublishNum()) {
            throw new ServiceException(ResultCode.COUPON_NUM_INSUFFICIENT_ERROR);
        }
        if (!coupon.getCouponLimitNum().equals(0) && haveCoupons >= coupon.getCouponLimitNum()) {
            throw new ServiceException(ResultCode.COUPON_LIMIT_ERROR, "此优惠券最多领取" + coupon.getCouponLimitNum() + "张");
        }
    }

    /**
     * 领取优惠券
     *
     * @param couponId   优惠券编号
     * @param memberId   会员
     * @param memberName 会员名称
     */
    @Override
    @CacheEvict(key = "#memberId")
    public void receiveBuyerCoupon(String couponId, String memberId, String memberName) {
        Coupon coupon = couponService.getById(couponId);
        if (coupon != null && !CouponGetEnum.FREE.name().equals(coupon.getGetType())) {
            throw new ServiceException(ResultCode.COUPON_DO_NOT_RECEIVER);
        } else if (coupon != null) {
            this.receiverCoupon(couponId, memberId, memberName, coupon);
        }

    }

    @Override
    @CacheEvict(key = "#memberId")
    public void receiveCoupon(String couponId, String memberId, String memberName) {
        Coupon coupon = couponService.getById(couponId);
        if (coupon != null) {
            this.receiverCoupon(couponId, memberId, memberName, coupon);
        } else {
            throw new ServiceException(ResultCode.COUPON_NOT_EXIST);
        }
    }

    @Override
    public IPage<MemberCoupon> getMemberCoupons(MemberCouponSearchParams param, PageVO pageVo) {
        QueryWrapper<MemberCoupon> queryWrapper = param.queryWrapper();
        return this.page(PageUtil.initPage(pageVo), queryWrapper);
    }

    /**
     * 获取会员优惠券列表
     *
     * @param param 查询参数
     * @return 会员优惠券列表
     */
    @Override
    public List<MemberCoupon> getMemberCoupons(MemberCouponSearchParams param) {
        return this.list(param.queryWrapper());
    }

    /**
     * 获取当前用户的优惠券列表（优先读取缓存）
     *
     * @param memberId 会员id
     * @return 会员优惠券列表
     */
    @Override
    @Cacheable(key = "#memberId")
    public List<MemberCoupon> getMemberCoupons(String memberId) {
        MemberCouponSearchParams searchParams = new MemberCouponSearchParams();
        searchParams.setMemberId(Objects.requireNonNull(UserContext.getCurrentUser()).getId());
        searchParams.setMemberCouponStatus(MemberCouponStatusEnum.NEW.name());
        searchParams.setPromotionStatus(PromotionsStatusEnum.START.name());
        return this.getMemberCoupons(searchParams);
    }

    /**
     * 获取会员优惠券列表
     *
     * @param param  查询参数
     * @param pageVo 分页参数
     * @return 会员优惠券列表
     */
    @Override
    public IPage<MemberCoupon> getMemberCouponsByCanUse(MemberCouponSearchParams param, Double totalPrice, PageVO pageVo) {
        LambdaQueryWrapper<MemberCoupon> queryWrapper = new LambdaQueryWrapper<>();
        List<String> storeIds = new ArrayList<>(Arrays.asList(param.getStoreId().split(",")));
        storeIds.add(PromotionTools.PLATFORM_ID);
        queryWrapper.in(MemberCoupon::getStoreId, storeIds);
        queryWrapper.eq(MemberCoupon::getMemberId, param.getMemberId());
        queryWrapper.and(
                i -> i.like(MemberCoupon::getScopeId, param.getScopeId())
                        .or(j -> j.eq(MemberCoupon::getScopeType, PromotionsScopeTypeEnum.ALL.name())));
        queryWrapper.eq(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.NEW.name());
        queryWrapper.le(MemberCoupon::getConsumeThreshold, totalPrice);
        queryWrapper.ge(MemberCoupon::getEndTime, new Date());
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
        queryWrapper.ne(MemberCoupon::getScopeType, PromotionsScopeTypeEnum.ALL.name());
        queryWrapper.le(MemberCoupon::getConsumeThreshold, totalPrice);
        queryWrapper.ge(MemberCoupon::getEndTime, new Date());
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
        queryWrapper.eq(MemberCoupon::getScopeType, PromotionsScopeTypeEnum.ALL.name());
        queryWrapper.ge(MemberCoupon::getEndTime, new Date()).and(i -> i.in(MemberCoupon::getStoreId, storeId).or(j -> j.eq(MemberCoupon::getPlatformFlag, true)));
        return this.list(queryWrapper);
    }

    /**
     * 获取当前会员全品类优惠券
     *
     * @param param 查询参数
     * @return 会员优惠券列表
     */
    @Override
    public MemberCoupon getMemberCoupon(MemberCouponSearchParams param) {
        return this.getOne(param.queryWrapper(), false);
    }

    @Override
    public long getMemberCouponsNum() {
        AuthUser authUser = Objects.requireNonNull(UserContext.getCurrentUser());
        QueryWrapper<MemberCoupon> queryWrapper = Wrappers.query();
        queryWrapper.eq("member_id", authUser.getId());
        queryWrapper.eq("member_coupon_status", MemberCouponStatusEnum.NEW.name());
        queryWrapper.eq("delete_flag", false);
        return this.count(queryWrapper);
    }


    @Override
    @CacheEvict(key = "#memberId")
    public void used(String memberId, List<String> ids) {
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
    @CacheEvict(key = "#memberId")
    public void cancellation(String memberId, String id) {
        LambdaUpdateWrapper<MemberCoupon> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(MemberCoupon::getId, id);
        updateWrapper.set(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.CLOSED.name());
        this.update(updateWrapper);
    }

    /**
     * 关闭会员优惠券
     *
     * @param couponIds 优惠券id集合
     */
    @Override
    public void closeMemberCoupon(List<String> couponIds) {
        LambdaUpdateWrapper<MemberCoupon> memberCouponLambdaUpdateWrapper = new LambdaUpdateWrapper<MemberCoupon>()
                .in(MemberCoupon::getCouponId, couponIds)
                .set(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.CLOSED.name());
        this.cache.vagueDel("{MemberCoupon}");
        this.update(memberCouponLambdaUpdateWrapper);
    }

    private void receiverCoupon(String couponId, String memberId, String memberName, Coupon coupon) {
        this.checkCouponLimit(couponId, memberId);
        MemberCoupon memberCoupon = new MemberCoupon(coupon);
        memberCoupon.setMemberId(memberId);
        memberCoupon.setMemberName(memberName);
        memberCoupon.setMemberCouponStatus(MemberCouponStatusEnum.NEW.name());
        memberCoupon.setPlatformFlag((PromotionTools.PLATFORM_ID).equals(coupon.getStoreId()));
        this.save(memberCoupon);
        couponService.receiveCoupon(couponId, 1);
    }
}