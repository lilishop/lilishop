package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.mapper.FullDiscountMapper;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.FullDiscountService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

/**
 * 满优惠业务层实现
 *
 * @author Chopper
 * @since 2020/8/21
 */
@Service
public class FullDiscountServiceImpl extends AbstractPromotionsServiceImpl<FullDiscountMapper, FullDiscount> implements FullDiscountService {

    /**
     * 优惠券
     */
    @Autowired
    private CouponService couponService;
    /**
     * 促销商品
     */
    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Override
    public List<FullDiscountVO> currentPromotion(List<String> storeId) {
        List<FullDiscountVO> result = new ArrayList<>();
        QueryWrapper<FullDiscount> queryWrapper = new QueryWrapper<>();
        queryWrapper.in(storeId != null && !storeId.isEmpty(), "store_id", storeId);
        queryWrapper.and(PromotionTools.queryPromotionStatus(PromotionsStatusEnum.START));
        List<FullDiscount> list = this.list(queryWrapper);
        if (list != null) {
            for (FullDiscount fullDiscount : list) {
                PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
                searchParams.setPromotionId(fullDiscount.getId());
                FullDiscountVO fullDiscountVO = new FullDiscountVO(fullDiscount);
                fullDiscountVO.setPromotionGoodsList(promotionGoodsService.listFindAll(searchParams));
                result.add(fullDiscountVO);
            }
        }
        return result;
    }

    /**
     * 获取满优惠活动详情
     *
     * @param id 满优惠KID
     * @return 满优惠活动详情
     */
    @Override
    public FullDiscountVO getFullDiscount(String id) {
        FullDiscount fullDiscount = this.checkFullDiscountExist(id);
        FullDiscountVO fullDiscountVO = new FullDiscountVO(fullDiscount);
        PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
        searchParams.setPromotionId(fullDiscount.getId());
        fullDiscountVO.setPromotionGoodsList(promotionGoodsService.listFindAll(searchParams));
        return fullDiscountVO;
    }

    /**
     * 检查促销参数
     *
     * @param promotions 促销实体
     */
    @Override
    public void checkPromotions(FullDiscount promotions) {
        super.checkPromotions(promotions);
        if (promotions instanceof FullDiscountVO) {
            FullDiscountVO fullDiscountVO = (FullDiscountVO) promotions;
            //验证是否是有效参数
            PromotionTools.checkPromotionTime(fullDiscountVO.getStartTime(), fullDiscountVO.getEndTime());
        }

        //当前时间段是否存在同类活动
        this.checkSameActiveExist(promotions.getStartTime(), promotions.getEndTime(), promotions.getStoreId(), promotions.getId());
        //检查满减参数
        this.checkFullDiscount(promotions);

    }

    /**
     * 更新促销商品信息
     *
     * @param promotions 促销实体
     * @return 是否更新成功
     */
    @Override
    @Transactional(rollbackFor = {Exception.class})
    public boolean updatePromotionsGoods(FullDiscount promotions) {
        boolean result = super.updatePromotionsGoods(promotions);
        if (!PromotionsStatusEnum.CLOSE.name().equals(promotions.getPromotionStatus())
                && PromotionsScopeTypeEnum.PORTION_GOODS.name().equals(promotions.getScopeType())
                && promotions instanceof FullDiscountVO) {
            FullDiscountVO fullDiscountVO = (FullDiscountVO) promotions;
            List<PromotionGoods> promotionGoodsList = PromotionTools.promotionGoodsInit(fullDiscountVO.getPromotionGoodsList(), fullDiscountVO, PromotionTypeEnum.FULL_DISCOUNT);
            this.promotionGoodsService.deletePromotionGoods(Collections.singletonList(promotions.getId()));
            //促销活动商品更新
            result = this.promotionGoodsService.saveBatch(promotionGoodsList);
        }
        return result;

    }

    /**
     * 更新促销信息到商品索引
     *
     * @param promotions 促销实体
     */
    @Override
    public void updateEsGoodsIndex(FullDiscount promotions) {
        FullDiscount fullDiscount = JSONUtil.parse(promotions).toBean(FullDiscount.class);
        super.updateEsGoodsIndex(fullDiscount);
    }

    /**
     * 当前促销类型
     *
     * @return 当前促销类型
     */
    @Override
    public PromotionTypeEnum getPromotionType() {
        return PromotionTypeEnum.FULL_DISCOUNT;
    }

    /**
     * 检查满优惠活动是否存在
     *
     * @param id 满优惠活动id
     * @return 满优惠活动
     */
    private FullDiscount checkFullDiscountExist(String id) {
        FullDiscount fullDiscount = this.getById(id);
        if (fullDiscount == null) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_NOT_EXIST_ERROR);
        }
        return fullDiscount;
    }

    /**
     * 检查满减参数
     *
     * @param fullDiscount 满减参数信息
     */
    private void checkFullDiscount(FullDiscount fullDiscount) {
        if (fullDiscount.getFullMinusFlag() == null && fullDiscount.getCouponFlag() == null && fullDiscount.getGiftFlag() == null
                && fullDiscount.getPointFlag() == null && fullDiscount.getFullRateFlag() == null) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_WAY_ERROR);
        }
        //如果优惠方式是满减
        if (Boolean.TRUE.equals(fullDiscount.getFullMinusFlag())) {
            this.checkFullMinus(fullDiscount.getFullMinus(), fullDiscount.getFullMoney());
            fullDiscount.setTitle("满" + fullDiscount.getFullMoney() + " 减" + fullDiscount.getFullMinus());
        }
        //如果优惠方式是赠品
        if (Boolean.TRUE.equals(fullDiscount.getGiftFlag())) {
            //是否没有选择赠品
            boolean noGiftSelected = fullDiscount.getGiftId() == null;
            if (noGiftSelected) {
                throw new ServiceException(ResultCode.FULL_DISCOUNT_GIFT_ERROR);
            }
        } else {
            fullDiscount.setGiftId(null);
        }
        //如果优惠方式是赠优惠券
        if (Boolean.TRUE.equals(fullDiscount.getCouponFlag())) {
            this.checkCoupon(fullDiscount.getCouponId());
        } else {
            fullDiscount.setCouponId(null);
        }
        //如果优惠方式是折扣
        if (Boolean.TRUE.equals(fullDiscount.getFullRateFlag())) {
            this.checkFullRate(fullDiscount.getFullRate());
            fullDiscount.setTitle("满" + fullDiscount.getFullMoney() + " 打" + fullDiscount.getFullRate() + "折");
        }

    }

    /**
     * 检查同一时间段内不能存在相同的活动数量
     *
     * @param statTime 开始时间
     * @param endTime  结束时间
     * @param storeId  店铺id
     * @param id       满优惠活动ID
     */
    private void checkSameActiveExist(Date statTime, Date endTime, String storeId, String id) {
        //同一时间段内相同的活动
        QueryWrapper<FullDiscount> queryWrapper = PromotionTools.checkActiveTime(statTime, endTime, PromotionTypeEnum.FULL_DISCOUNT, storeId, id);
        long sameNum = this.count(queryWrapper);
        if (sameNum > 0) {
            throw new ServiceException(ResultCode.PROMOTION_SAME_ACTIVE_EXIST);
        }
    }

    /**
     * 检查优惠券信息
     *
     * @param couponId 优惠券编号
     */
    private void checkCoupon(String couponId) {
        //是否没有选择优惠券
        boolean noCouponSelected = couponId == null;
        if (noCouponSelected) {
            throw new ServiceException(ResultCode.COUPON_NOT_EXIST);
        }
        Coupon coupon = this.couponService.getById(couponId);
        if (coupon == null) {
            throw new ServiceException(ResultCode.COUPON_NOT_EXIST);
        }
    }

    /**
     * 检查满减信息
     *
     * @param fullMinus 满减金额
     * @param fullMoney 优惠门槛
     */
    private void checkFullMinus(Double fullMinus, Double fullMoney) {
        //是否没有填写满减金额
        boolean noFullMinusInput = fullMinus == null || fullMinus == 0;
        if (noFullMinusInput) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_MONEY_ERROR);
        }
        if (fullMinus > fullMoney) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_MONEY_GREATER_THAN_MINUS);
        }
    }

    /**
     * 检查打折信息
     *
     * @param fullRate 打折数值
     */
    private void checkFullRate(Double fullRate) {
        //是否没有填写打折数值
        boolean noFullRateInput = fullRate == null || fullRate == 0;
        if (noFullRateInput) {
            throw new ServiceException(ResultCode.FULL_RATE_NUM_ERROR);
        }
        int rateLimit = 10;
        if (fullRate >= rateLimit || fullRate <= 0) {
            throw new ServiceException(ResultCode.FULL_RATE_NUM_ERROR);
        }
    }
}