package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.support.GoodsSkuReadSupport;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.search.CouponSearchParams;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.enums.CouponRangeDayEnum;
import cn.lili.modules.promotion.entity.enums.CouponTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.entity.vos.CouponVO;
import cn.lili.modules.promotion.mapper.CouponMapper;
import cn.lili.modules.promotion.service.*;
import cn.lili.modules.promotion.support.CouponPromotionReferenceSupport;
import cn.lili.modules.promotion.support.CouponReceiveSupport;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 优惠券活动业务层实现
 * 商品读取和满额活动引用校验复用独立支持组件，避免依赖完整商品、满额活动服务。
 *
 * @author Chopper
 * @since 2020/8/21
 */
@Service
public class CouponServiceImpl extends AbstractPromotionsServiceImpl<CouponMapper, Coupon> implements CouponService {

    @Autowired
    private GoodsSkuReadSupport skuReads;
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    @Autowired
    private MemberCouponService memberCouponService;
    @Autowired
    private CouponPromotionReferenceSupport couponReferences;
    @Autowired
    private CouponReceiveSupport couponReceive;
    @Autowired
    private CouponActivityItemService couponActivityItemService;

    /**
     * 领取优惠券
     * 发行量保护复用基础组件，已领数量按SQL原子增加，沿用调用方事务。
     *
     * @param couponId   优惠券id
     * @param receiveNum 领取数量
     */
    @Override
    public void receiveCoupon(String couponId, Integer receiveNum) {
        couponReceive.receiveCoupon(couponId, receiveNum);
    }

    @Override
    @Transactional(rollbackFor = {Exception.class})
    public boolean removePromotions(List<String> ids) {
        //删除优惠券信息
        this.memberCouponService.closeMemberCoupon(ids);

        //删除优惠券活动关联优惠券
        this.couponActivityItemService.removeByCouponId(ids);
        return super.removePromotions(ids);
    }

    /**
     * 使用优惠券
     *
     * @param couponId 优惠券id
     * @param usedNum  使用数量
     */
    @Override
    public void usedCoupon(String couponId, Integer usedNum) {
        Coupon coupon = this.getById(couponId);
        if (coupon == null) {
            throw new ServiceException(ResultCode.COUPON_NOT_EXIST);
        }

        this.update(new LambdaUpdateWrapper<Coupon>().eq(Coupon::getId, coupon.getId()).set(Coupon::getUsedNum,
                coupon.getUsedNum() + usedNum));
    }

    /**
     * 获取优惠券展示实体
     *
     * @param searchParams 查询参数
     * @param page         分页参数
     * @return 优惠券展示实体列表
     */
    @Override
    public IPage<CouponVO> pageVOFindAll(CouponSearchParams searchParams, PageVO page) {
        IPage<Coupon> couponIPage = super.pageFindAll(searchParams, page);
        List<CouponVO> couponVOList = couponIPage.getRecords().stream().map(CouponVO::new).collect(Collectors.toList());
        return PageUtil.convertPage(couponIPage, couponVOList);
    }

    /**
     * 获取优惠券展示详情
     *
     * @param couponId 优惠券id
     * @return 返回优惠券展示详情
     */
    @Override
    public CouponVO getDetail(String couponId) {
        CouponVO couponVO = new CouponVO(this.getById(couponId));
        PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
        searchParams.setPromotionId(couponId);
        List<PromotionGoods> promotionsByPromotionId = this.promotionGoodsService.listFindAll(searchParams);
        if (promotionsByPromotionId != null && !promotionsByPromotionId.isEmpty()) {
            couponVO.setPromotionGoodsList(promotionsByPromotionId);
        }
        return couponVO;
    }

    @Override
    public List<String> filterCouponIdsByStoreId(List<String> couponIds, String storeId) {
        if (couponIds == null || couponIds.isEmpty()) {
            return Collections.emptyList();
        }
        return this.list(new LambdaQueryWrapper<Coupon>()
                .in(Coupon::getId, couponIds)
                .eq(Coupon::getStoreId, storeId)).stream().map(Coupon::getId).collect(Collectors.toList());
    }

    /**
     * 更新促销状态
     * 如果要更新促销状态为关闭，startTime和endTime置为空即可
     *
     * @param ids       促销id集合
     * @param startTime 开始时间
     * @param endTime   结束时间
     * @return 是否更新成功
     */
    @Override
    @Transactional(rollbackFor = {Exception.class})
    public boolean updateStatus(List<String> ids, Long startTime, Long endTime) {
        List<Coupon> list = this.list(new LambdaQueryWrapper<Coupon>().in(Coupon::getId, ids).eq(Coupon::getRangeDayType, CouponRangeDayEnum.DYNAMICTIME.name()));
        if (!list.isEmpty()) {
            LambdaUpdateWrapper<Coupon> updateWrapper = new LambdaUpdateWrapper<>();
            updateWrapper.in(Coupon::getId, list.stream().map(Coupon::getId).collect(Collectors.toList()));
            updateWrapper.set(Coupon::getEffectiveDays, 0);
            this.update(updateWrapper);
        }

        // 关闭优惠券，删除相关客户优惠券和券活动
        if (startTime == null && endTime == null) {
            //删除优惠券信息
            this.memberCouponService.closeMemberCoupon(ids);
            //删除优惠券活动关联优惠券
            this.couponActivityItemService.removeByCouponId(ids);
        }
        return super.updateStatus(ids, startTime, endTime);
    }

    @Override
    public void initPromotion(Coupon promotions) {
        promotions.setUsedNum(0);
        promotions.setReceivedNum(0);
    }

    @Override
    public void checkPromotions(Coupon coupon) {
        if (coupon.getRangeDayType() == null) {
            super.checkPromotions(coupon);
        }
        //优惠券限制领取数量
        if (coupon.getCouponLimitNum() < 0) {
            throw new ServiceException(ResultCode.COUPON_LIMIT_NUM_LESS_THAN_0);
        }
        //如果发行数量是0则判断领取限制数量
        if (coupon.getPublishNum() != 0 && coupon.getCouponLimitNum() > coupon.getPublishNum()) {
            throw new ServiceException(ResultCode.COUPON_LIMIT_GREATER_THAN_PUBLISH);
        }
        // PRICE + 零门槛表示立减券，沿用现金券领取和结算链路。
        if (!CouponTypeEnum.PRICE.name().equals(coupon.getCouponType())
                && !CouponTypeEnum.DISCOUNT.name().equals(coupon.getCouponType())) {
            throw new ServiceException("优惠券类型不合法");
        }
        if (!validCouponAmount(coupon.getConsumeThreshold(), true)) {
            throw new ServiceException("消费门槛必须为0至99999999之间的金额，最多两位小数");
        }
        if (CouponTypeEnum.PRICE.name().equals(coupon.getCouponType())) {
            if (!validCouponAmount(coupon.getPrice(), false)) {
                throw new ServiceException("减免金额必须大于0且不超过99999999，最多两位小数");
            }
            coupon.setCouponDiscount(null);
        } else {
            Double discount = coupon.getCouponDiscount();
            if (discount == null || !Double.isFinite(discount) || discount < 0.1 || discount > 9.9
                    || java.math.BigDecimal.valueOf(discount).stripTrailingZeros().scale() > 1) {
                throw new ServiceException(ResultCode.COUPON_DISCOUNT_ERROR);
            }
            if (coupon.getConsumeThreshold() <= 0) {
                throw new ServiceException("满折优惠券的消费门槛必须大于0");
            }
            coupon.setPrice(null);
        }

        //如果优惠券使用时间类型不合法，抛出异常，抛出异常
        if (!CouponRangeDayEnum.exist(coupon.getRangeDayType())) {
            throw new ServiceException(ResultCode.COUPON_RANGE_ERROR);
        }

        switch (CouponRangeDayEnum.valueOf(coupon.getRangeDayType())) {
            case FIXEDTIME:
                //如果优惠券为固定时间，则开始结束时间不能为空
                if (coupon.getEndTime() == null || coupon.getStartTime() == null) {
                    throw new ServiceException(ResultCode.PROMOTION_TIME_ERROR);
                }
                long nowTime = DateUtil.getDateline() * 1000;
                //固定时间的优惠券不能小于当前时间
                if (coupon.getEndTime().getTime() < nowTime) {
                    throw new ServiceException(ResultCode.PROMOTION_END_TIME_ERROR);
                }
                break;
            case DYNAMICTIME:
                //固定时间的优惠券不能小于当前时间
                if (coupon.getEffectiveDays() == null || coupon.getEffectiveDays() < 0) {
                    throw new ServiceException(ResultCode.PROMOTION_END_TIME_ERROR);
                }
                break;
        }


        this.checkCouponScope((CouponVO) coupon);
    }

    /**
     * 校验优惠券金额为有限数值、不超过99999999元且最多两位小数。
     *
     * @param amount 待校验金额，单位元
     * @param allowZero 是否允许零金额
     * @return 金额符合范围及精度要求时为true
     */
    private boolean validCouponAmount(Double amount, boolean allowZero) {
        return amount != null && Double.isFinite(amount) && (allowZero ? amount >= 0 : amount > 0)
                && amount <= 99999999
                && java.math.BigDecimal.valueOf(amount).stripTrailingZeros().scale() <= 2;
    }

    @Override
    public void checkStatus(Coupon promotions) {
        super.checkStatus(promotions);
        couponReferences.checkCouponNotUsedByActiveFullDiscount(promotions.getId());
    }

    @Override
    @Transactional(rollbackFor = {Exception.class})
    public boolean updatePromotionsGoods(Coupon promotions) {
        boolean result = super.updatePromotionsGoods(promotions);
        if (!PromotionsStatusEnum.CLOSE.name().equals(promotions.getPromotionStatus()) &&
                PromotionsScopeTypeEnum.PORTION_GOODS.name().equals(promotions.getScopeType()) &&
                promotions instanceof CouponVO) {
            CouponVO couponVO = (CouponVO) promotions;
            this.promotionGoodsService.deletePromotionGoods(Collections.singletonList(promotions.getId()));
            List<PromotionGoods> promotionGoodsList = PromotionTools.promotionGoodsInit(couponVO.getPromotionGoodsList(), couponVO, this.getPromotionType());
            for (PromotionGoods promotionGoods : promotionGoodsList) {
                promotionGoods.setStoreId(promotions.getStoreId());
                promotionGoods.setStoreName(promotions.getStoreName());
            }
            //促销活动商品更新
            result = this.promotionGoodsService.saveBatch(promotionGoodsList);
        }
        return result;
    }

    /**
     * 更新商品索引优惠券信息
     *
     * @param promotions 优惠券信息
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsIndex(Coupon promotions) {
        // Elasticsearch 已移除，促销信息在搜索时实时组装
    }

    @Override
    public PromotionTypeEnum getPromotionType() {
        return PromotionTypeEnum.COUPON;
    }

    /**
     * 检查优惠券范围
     *
     * @param coupon 检查的优惠券对象
     */
    private void checkCouponScope(CouponVO coupon) {
        boolean portionGoodsScope = (coupon.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_GOODS.name())
                && (coupon.getPromotionGoodsList() == null || coupon.getPromotionGoodsList().isEmpty()));
        if (portionGoodsScope) {
            throw new ServiceException(ResultCode.COUPON_SCOPE_TYPE_GOODS_ERROR);
        } else if (coupon.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_GOODS.name()) && CharSequenceUtil.isEmpty(coupon.getScopeId())) {
            throw new ServiceException(ResultCode.COUPON_SCOPE_TYPE_GOODS_ERROR);
        } else if (coupon.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_GOODS_CATEGORY.name()) && CharSequenceUtil.isEmpty(coupon.getScopeId())) {
            throw new ServiceException(ResultCode.COUPON_SCOPE_TYPE_CATEGORY_ERROR);
        } else if (coupon.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_SHOP_CATEGORY.name()) && CharSequenceUtil.isEmpty(coupon.getScopeId())) {
            throw new ServiceException(ResultCode.COUPON_SCOPE_TYPE_STORE_ERROR);
        }

        if (coupon.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_GOODS.name())) {
            this.checkCouponPortionGoods(coupon);
        }
    }

    /**
     * 检查指定商品
     *
     * @param coupon 优惠券信息
     */
    private void checkCouponPortionGoods(CouponVO coupon) {
        String[] split = coupon.getScopeId().split(",");
        if (split.length == 0) {
            throw new ServiceException(ResultCode.COUPON_SCOPE_ERROR);
        }
        for (String id : split) {
            GoodsSku goodsSku = skuReads.getGoodsSkuByIdFromCache(id);
            if (goodsSku == null) {
                throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
            }
        }
    }

}
