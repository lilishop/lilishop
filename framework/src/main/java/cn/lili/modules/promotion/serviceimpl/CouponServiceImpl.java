package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.search.CouponSearchParams;
import cn.lili.modules.promotion.entity.dto.search.FullDiscountSearchParams;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.enums.CouponRangeDayEnum;
import cn.lili.modules.promotion.entity.enums.CouponTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.entity.vos.CouponVO;
import cn.lili.modules.promotion.mapper.CouponMapper;
import cn.lili.modules.promotion.service.*;
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
 *
 * @author Chopper
 * @since 2020/8/21
 */
@Service
public class CouponServiceImpl extends AbstractPromotionsServiceImpl<CouponMapper, Coupon> implements CouponService {

    /**
     * 规格商品
     */
    @Autowired
    private GoodsSkuService goodsSkuService;
    /**
     * 促销商品
     */
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    /**
     * 会员优惠券
     */
    @Autowired
    private MemberCouponService memberCouponService;
    /**
     * 满额活动
     */
    @Autowired
    private FullDiscountService fullDiscountService;
    /**
     * 优惠券活动-优惠券关联
     */
    @Autowired
    private CouponActivityItemService couponActivityItemService;

    /**
     * 领取优惠券
     *
     * @param couponId   优惠券id
     * @param receiveNum 领取数量
     */
    @Override
    public void receiveCoupon(String couponId, Integer receiveNum) {
        Coupon coupon = this.getById(couponId);
        if (coupon == null) {
            throw new ServiceException(ResultCode.COUPON_NOT_EXIST);
        }
        this.update(new LambdaUpdateWrapper<Coupon>().eq(Coupon::getId, coupon.getId()).set(Coupon::getReceivedNum,
                coupon.getReceivedNum() + receiveNum));
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

        return super.updateStatus(ids, startTime, endTime);
    }

    @Override
    public void initPromotion(Coupon promotions) {
        promotions.setUsedNum(0);
        promotions.setReceivedNum(0);
    }

    @Override
    public void checkPromotions(Coupon promotions) {
        if (promotions.getRangeDayType() == null) {
            super.checkPromotions(promotions);
        }
        //优惠券限制领取数量
        if (promotions.getCouponLimitNum() < 0) {
            throw new ServiceException(ResultCode.COUPON_LIMIT_NUM_LESS_THAN_0);
        }
        //如果发行数量是0则判断领取限制数量
        if (promotions.getPublishNum() != 0 && promotions.getCouponLimitNum() > promotions.getPublishNum()) {
            throw new ServiceException(ResultCode.COUPON_LIMIT_GREATER_THAN_PUBLISH);
        }
        //打折优惠券大于10折
        boolean discountCoupon = (promotions.getCouponType().equals(CouponTypeEnum.DISCOUNT.name())
                && (promotions.getCouponDiscount() < 0 && promotions.getCouponDiscount() > 10));
        if (discountCoupon) {
            throw new ServiceException(ResultCode.COUPON_DISCOUNT_ERROR);
        }

        //优惠券为固定时间类型
        if (promotions.getRangeDayType() != null && promotions.getRangeDayType().equals(CouponRangeDayEnum.FIXEDTIME.name())) {
            long nowTime = DateUtil.getDateline() * 1000;
            //固定时间的优惠券不能小于当前时间
            if (promotions.getEndTime().getTime() < nowTime) {
                throw new ServiceException(ResultCode.PROMOTION_END_TIME_ERROR);
            }
        }


        this.checkCouponScope((CouponVO) promotions);
    }

    @Override
    public void checkStatus(Coupon promotions) {
        super.checkStatus(promotions);
        FullDiscountSearchParams searchParams = new FullDiscountSearchParams();
        searchParams.setCouponFlag(true);
        searchParams.setCouponId(promotions.getId());
        searchParams.setPromotionStatus(PromotionsStatusEnum.START.name());
        List<FullDiscount> fullDiscounts = fullDiscountService.listFindAll(searchParams);
        if (fullDiscounts != null && !fullDiscounts.isEmpty()) {
            throw new ServiceException("当前优惠券参与了促销活动【" + fullDiscounts.get(0).getPromotionName() + "】不能进行编辑删除操作");
        }
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
    public void updateEsGoodsIndex(Coupon promotions) {
        Coupon coupon = JSONUtil.parse(promotions).toBean(Coupon.class);
        super.updateEsGoodsIndex(coupon);
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
        if (split.length <= 0) {
            throw new ServiceException(ResultCode.COUPON_SCOPE_ERROR);
        }
        for (String id : split) {
            GoodsSku goodsSku = goodsSkuService.getCanPromotionGoodsSkuByIdFromCache(id);
            if (goodsSku == null) {
                throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
            }
        }
    }

}