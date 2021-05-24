package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.delayqueue.PromotionMessage;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.DateUtil;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.promotion.entity.dos.*;
import cn.lili.modules.promotion.entity.enums.*;
import cn.lili.modules.promotion.entity.vos.*;
import cn.lili.modules.promotion.service.*;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.service.EsGoodsIndexService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * 促销业务层实现
 *
 * @author Chopper
 * @date 2020/8/21
 */
@Slf4j
@Service
@Transactional(rollbackFor = Exception.class)
public class PromotionServiceImpl implements PromotionService {
    //会员优惠券
    @Autowired
    private MemberCouponService memberCouponService;
    //秒杀
    @Autowired
    private SeckillService seckillService;
    //秒杀申请
    @Autowired
    private SeckillApplyService seckillApplyService;
    //满额活动
    @Autowired
    private FullDiscountService fullDiscountService;
    //拼团
    @Autowired
    private PintuanService pintuanService;
    //优惠券
    @Autowired
    private CouponService couponService;
    //促销商品
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    //积分商品
    @Autowired
    private PointsGoodsService pointsGoodsService;
    //优惠券活动
    @Autowired
    private CouponActivityService couponActivityService;
    //ES商品
    @Autowired
    private EsGoodsIndexService goodsIndexService;
    //Mongo
    @Autowired
    private MongoTemplate mongoTemplate;


    @Override
    public boolean updatePromotionStatus(PromotionMessage promotionMessage) {
        PromotionTypeEnum promotionTypeEnum = PromotionTypeEnum.valueOf(promotionMessage.getPromotionType());
        String esPromotionKey = promotionTypeEnum + "-" + promotionMessage.getPromotionId();
        log.info("更新促销活动状态：{}", promotionMessage);
        boolean result = false;
        switch (promotionTypeEnum) {
            //满减
            case FULL_DISCOUNT:
                result = this.updateFullDiscount(promotionMessage, esPromotionKey, promotionTypeEnum);
                break;
            //秒杀
            case SECKILL:
                result = this.updateSeckill(promotionMessage, esPromotionKey, promotionTypeEnum);
                break;
            //拼团
            case PINTUAN:
                result = this.updatePintuan(promotionMessage, esPromotionKey, promotionTypeEnum);
                break;
            //优惠券
            case COUPON:
                result = this.updateCoupon(promotionMessage, esPromotionKey, promotionTypeEnum);
                break;
            //积分商品
            case POINTS_GOODS:
                result = this.updatePointsGoods(promotionMessage, esPromotionKey, promotionTypeEnum);
                break;
            //优惠券活动
            case COUPON_ACTIVITY:
                result = this.updateCouponActivity(promotionMessage, promotionTypeEnum);
                break;
            default:
                break;
        }
        this.updatePromotionGoods(promotionMessage.getPromotionId(), promotionMessage.getPromotionStatus());
        return result;
    }

    /**
     * 获取当前进行的所有促销活动信息
     *
     * @return 当前促销活动集合
     */
    @Override
    public Map<String, Object> getCurrentPromotion() {
        Map<String, Object> resultMap = new HashMap<>();
        QueryWrapper queryWrapper = new QueryWrapper();
        queryWrapper.eq("promotion_status", PromotionStatusEnum.START.name());
        queryWrapper.gt("start_time", new Date());
        queryWrapper.lt("end_time", new Date());
        // 获取当前进行的秒杀活动活动
        List<Seckill> seckillList = seckillService.list(queryWrapper);
        if (seckillList != null && !seckillList.isEmpty()) {
            for (Seckill seckill : seckillList) {
                resultMap.put(PromotionTypeEnum.SECKILL.name(), seckill);
            }
        }
        // 获取当前进行的满优惠活动
        List<FullDiscount> fullDiscountList = fullDiscountService.list(queryWrapper);
        if (fullDiscountList != null && !fullDiscountList.isEmpty()) {
            for (FullDiscount fullDiscount : fullDiscountList) {
                resultMap.put(PromotionTypeEnum.FULL_DISCOUNT.name(), fullDiscount);
            }
        }
        // 获取当前进行的拼团活动
        List<Pintuan> pintuanList = pintuanService.list(queryWrapper);
        if (pintuanList != null && !pintuanList.isEmpty()) {
            for (Pintuan pintuan : pintuanList) {
                resultMap.put(PromotionTypeEnum.PINTUAN.name(), pintuan);
            }
        }
        return resultMap;
    }

    /**
     * 根据商品索引获取当前商品索引的所有促销活动信息
     *
     * @param index 商品索引
     * @return 当前促销活动集合
     */
    @Override
    public Map<String, Object> getGoodsCurrentPromotionMap(EsGoodsIndex index) {
        Map<String, Object> promotionMap = new HashMap<>();
        Query query = new Query();
        query.addCriteria(Criteria.where("deleteFlag").is(false));
        query.addCriteria(Criteria.where("promotionStatus").is(PromotionStatusEnum.START.name()));
        query.addCriteria(Criteria.where("endTime").gt(new Date()));
        List<FullDiscountVO> fullDiscountVOS = mongoTemplate.find(query, FullDiscountVO.class);
        for (FullDiscountVO fullDiscountVO : fullDiscountVOS) {
            if (fullDiscountVO.getPromotionGoodsList() == null && fullDiscountVO.getNumber() == -1) {
                if (index.getStoreId().equals(fullDiscountVO.getStoreId())) {
                    String fullDiscountKey = PromotionTypeEnum.FULL_DISCOUNT.name() + "-" + fullDiscountVO.getId();
                    promotionMap.put(fullDiscountKey, fullDiscountVO);
                }
            }
        }
        List<CouponVO> couponVOS = mongoTemplate.find(query, CouponVO.class);
        for (CouponVO couponVO : couponVOS) {
            if (couponVO.getPromotionGoodsList() == null && couponVO.getScopeType().equals(CouponScopeTypeEnum.ALL.name())) {
                if (couponVO.getStoreId().equals("platform") || index.getStoreId().equals(couponVO.getStoreId())) {
                    String couponKey = PromotionTypeEnum.COUPON.name() + "-" + couponVO.getId();
                    promotionMap.put(couponKey, couponVO);
                }
            }
        }
        LambdaQueryWrapper<PromotionGoods> queryWrapper1 = new LambdaQueryWrapper<>();
        queryWrapper1.eq(PromotionGoods::getDeleteFlag, false);
        queryWrapper1.eq(PromotionGoods::getPromotionStatus, PromotionStatusEnum.START.name());
        queryWrapper1.gt(PromotionGoods::getEndTime, new Date());
        queryWrapper1.eq(PromotionGoods::getSkuId, index.getId());
        List<PromotionGoods> list1 = promotionGoodsService.list(queryWrapper1);
        for (PromotionGoods promotionGoods : list1) {
            String esPromotionKey = promotionGoods.getPromotionType() + "-" + promotionGoods.getPromotionId();
            switch (PromotionTypeEnum.valueOf(promotionGoods.getPromotionType())) {
                case COUPON:
                    Coupon coupon = couponService.getById(promotionGoods.getPromotionId());
                    promotionMap.put(esPromotionKey, coupon);
                    break;
                case PINTUAN:
                    Pintuan pintuan = pintuanService.getById(promotionGoods.getPromotionId());
                    promotionMap.put(esPromotionKey, pintuan);
                    index.setPromotionPrice(promotionGoods.getPrice());
                    break;
                case FULL_DISCOUNT:
                    FullDiscount fullDiscount = fullDiscountService.getById(promotionGoods.getPromotionId());
                    promotionMap.put(esPromotionKey, fullDiscount);
                    break;
                case SECKILL:
                    Seckill seckill = seckillService.getById(promotionGoods.getPromotionId());
                    LambdaQueryWrapper<SeckillApply> seckillApplyLambdaQueryWrapper = new LambdaQueryWrapper<>();
                    seckillApplyLambdaQueryWrapper.eq(SeckillApply::getSeckillId, promotionGoods.getPromotionId());
                    seckillApplyLambdaQueryWrapper.eq(SeckillApply::getSkuId, promotionGoods.getSkuId());
                    SeckillApply seckillApply = seckillApplyService.getOne(seckillApplyLambdaQueryWrapper);
                    int nextHour = 23;
                    String[] split = seckill.getHours().split(",");
                    int[] hoursSored = Arrays.stream(split).mapToInt(Integer::parseInt).toArray();
                    Arrays.sort(hoursSored);
                    for (int i : hoursSored) {
                        if (seckillApply.getTimeLine() < i) {
                            nextHour = i;
                        }
                    }
                    String seckillKey = promotionGoods.getPromotionType() + "-" + nextHour;
                    seckill.setStartTime(promotionGoods.getStartTime());
                    seckill.setEndTime(promotionGoods.getEndTime());
                    promotionMap.put(seckillKey, seckill);
                    index.setPromotionPrice(promotionGoods.getPrice());
                    break;
                case POINTS_GOODS:
                    PointsGoods pointsGoods = pointsGoodsService.getById(promotionGoods.getPromotionId());
                    promotionMap.put(esPromotionKey, pointsGoods);
                    break;
                default:
                    break;
            }
        }
        return promotionMap;
    }

    /**
     * 修改满额活动状态
     * @param promotionMessage 信息队列传输促销信息实体
     * @param esPromotionKey es Key
     * @param promotionTypeEnum 促销分类枚举
     * @return 修改结果
     */
    private boolean updateFullDiscount(PromotionMessage promotionMessage, String esPromotionKey, PromotionTypeEnum promotionTypeEnum) {
        boolean result;
        //从mongo中获取促销备份
        FullDiscountVO fullDiscountVO = mongoTemplate.findById(promotionMessage.getPromotionId(), FullDiscountVO.class);
        if (fullDiscountVO == null) {
            this.throwPromotionException(promotionTypeEnum, promotionMessage.getPromotionId(), promotionMessage.getPromotionStatus());
            return false;
        }
        //写入促销状态
        fullDiscountVO.setPromotionStatus(promotionMessage.getPromotionStatus());
        //修改促销数据
        result = this.fullDiscountService.update(promotionMessage.updateWrapper());
        // clone一个活动信息，用于存放与索引中
        FullDiscountVO clone = ObjectUtil.clone(fullDiscountVO);
        clone.setPromotionGoodsList(null);
        if (fullDiscountVO.getPromotionGoodsList() == null && fullDiscountVO.getNumber() == -1) {
            // 如果为全品类则更新全部索引
            this.goodsIndexService.updateEsGoodsIndexAllByList(clone, esPromotionKey);
        } else {
            // 如不为全品类，更新指定索引
            for (PromotionGoods promotionGoods : fullDiscountVO.getPromotionGoodsList()) {
                promotionGoods.setPromotionStatus(promotionMessage.getPromotionStatus());
            }
            this.promotionGoodsService.updateBatchById(fullDiscountVO.getPromotionGoodsList());
            this.goodsIndexService.updateEsGoodsIndexByList(fullDiscountVO.getPromotionGoodsList(), clone, esPromotionKey);
        }
        this.mongoTemplate.save(fullDiscountVO);
        return result;
    }

    /**
     * 修改优惠券状态
     * @param promotionMessage 信息队列传输促销信息实体
     * @param esPromotionKey es Key
     * @param promotionTypeEnum 促销分类枚举
     * @return 修改结果
     */
    private boolean updateCoupon(PromotionMessage promotionMessage, String esPromotionKey, PromotionTypeEnum promotionTypeEnum) {
        boolean result;
        //从mongo中获取优惠券信息
        CouponVO couponVO = this.mongoTemplate.findById(promotionMessage.getPromotionId(), CouponVO.class);
        if (couponVO == null) {
            this.throwPromotionException(promotionTypeEnum, promotionMessage.getPromotionId(), promotionMessage.getPromotionStatus());
            return false;
        }
        //修改优惠券
        couponVO.setPromotionStatus(promotionMessage.getPromotionStatus());
        result = this.couponService.update(promotionMessage.updateWrapper());
        //优惠券活动结束，会员已领取未使用的优惠券状态修改为：已过期
        if(couponVO.getPromotionStatus().equals(PromotionStatusEnum.END)){
            LambdaUpdateWrapper<MemberCoupon> updateWrapper = new LambdaUpdateWrapper<MemberCoupon>()
                    .eq(MemberCoupon::getCouponId, couponVO.getId())
                    .eq(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.NEW.name())
                    .set(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.EXPIRE.name());
            this.memberCouponService.update(updateWrapper);
        }
        // clone一个活动信息，用于存放与索引中
        CouponVO clone = ObjectUtil.clone(couponVO);
        clone.setPromotionGoodsList(null);
        if (CouponScopeTypeEnum.PORTION_GOODS.name().equals(couponVO.getScopeType())) {
            // 如为部分商品，则更新部分商品索引
            this.promotionGoodsService.updateBatchById(couponVO.getPromotionGoodsList());
            this.goodsIndexService.updateEsGoodsIndexByList(couponVO.getPromotionGoodsList(), clone, esPromotionKey);
        } else if (CouponScopeTypeEnum.ALL.name().equals(couponVO.getScopeType())) {
            // 如为全部，则更新全部商品索引
            this.goodsIndexService.updateEsGoodsIndexAllByList(clone, esPromotionKey);
        }
        this.mongoTemplate.save(couponVO);
        return result;
    }

    /**
     * 修改拼团状态
     * @param promotionMessage 信息队列传输促销信息实体
     * @param esPromotionKey es Key
     * @param promotionTypeEnum 促销分类枚举
     * @return 修改结果
     */
    private boolean updatePintuan(PromotionMessage promotionMessage, String esPromotionKey, PromotionTypeEnum promotionTypeEnum){
        boolean result;
        PintuanVO pintuanVO = this.mongoTemplate.findById(promotionMessage.getPromotionId(), PintuanVO.class);
        if (pintuanVO == null) {
            this.throwPromotionException(promotionTypeEnum, promotionMessage.getPromotionId(), promotionMessage.getPromotionStatus());
            return false;
        }
        pintuanVO.setPromotionStatus(promotionMessage.getPromotionStatus());
        result = this.pintuanService.update(promotionMessage.updateWrapper());
        this.promotionGoodsService.updateBatchById(pintuanVO.getPromotionGoodsList());
        List<PromotionGoods> promotionGoodsList = pintuanVO.getPromotionGoodsList();
        // 更新促销商品索引
        for (PromotionGoods promotionGoods : promotionGoodsList) {
            Pintuan pintuan1 = JSONUtil.toBean(JSONUtil.toJsonStr(pintuanVO), Pintuan.class);
            this.goodsIndexService.updateEsGoodsIndex(promotionGoods.getSkuId(), pintuan1, esPromotionKey, promotionGoods.getPrice());
        }
        this.mongoTemplate.save(pintuanVO);
        return result;
    }

    /**
     * 修改秒杀状态
     * @param promotionMessage 信息队列传输促销信息实体
     * @param esPromotionKey es Key
     * @param promotionTypeEnum 促销分类枚举
     * @return 修改结果
     */
    private boolean updateSeckill(PromotionMessage promotionMessage, String esPromotionKey, PromotionTypeEnum promotionTypeEnum){
        boolean result;
        SeckillVO seckill = this.mongoTemplate.findById(promotionMessage.getPromotionId(), SeckillVO.class);
        if (seckill == null) {
            this.throwPromotionException(promotionTypeEnum, promotionMessage.getPromotionId(), promotionMessage.getPromotionStatus());
            return false;
        }
        seckill.setPromotionStatus(promotionMessage.getPromotionStatus());
        result = this.seckillService.update(promotionMessage.updateWrapper());
        for (SeckillApply seckillApply : seckill.getSeckillApplyList()) {
            if (seckillApply.getPromotionApplyStatus().equals(PromotionApplyStatusEnum.PASS.name())) {
                // 下一个时间，默认为当天结束时间
                int nextHour = 23;
                String[] split = seckill.getHours().split(",");
                int[] hoursSored = Arrays.stream(split).mapToInt(Integer::parseInt).toArray();
                // 排序时间段
                Arrays.sort(hoursSored);
                for (int i : hoursSored) {
                    // 如果当前时间段大于排序后的时间段的某个，当前时间段的下个时间段即为排序后的时间段的某个
                    if (seckillApply.getTimeLine() < i) {
                        nextHour = i;
                        break;
                    }
                }
                Seckill seckill1 = JSONUtil.toBean(JSONUtil.toJsonStr(seckill), Seckill.class);
                String format = cn.hutool.core.date.DateUtil.format(seckill.getStartTime(), DateUtil.STANDARD_DATE_FORMAT);
                DateTime parseStartTime = cn.hutool.core.date.DateUtil.parse((format + " " + seckillApply.getTimeLine()), "yyyy-MM-dd HH");
                DateTime parseEndTime = cn.hutool.core.date.DateUtil.parse((format + " " + nextHour), "yyyy-MM-dd HH");
                // 如果是当天最后的时间段则设置到当天结束时间的59分59秒
                if (nextHour == seckillApply.getTimeLine()) {
                    parseEndTime = cn.hutool.core.date.DateUtil.parse((format + " " + nextHour + ":59:59"), DateUtil.STANDARD_FORMAT);
                }
                seckill1.setStartTime(parseStartTime);
                // 当时商品的秒杀活动活动结束时间为下个时间段的开始
                seckill1.setEndTime(parseEndTime);
                this.goodsIndexService.updateEsGoodsIndex(seckillApply.getSkuId(), seckill1, promotionTypeEnum.name() + "-" + seckillApply.getTimeLine(), seckillApply.getPrice());
            }
        }
        return result;
    }

    /**
     * 修改积分商品状态
     * @param promotionMessage 信息队列传输促销信息实体
     * @param esPromotionKey es Key
     * @param promotionTypeEnum 促销分类枚举
     * @return 修改结果
     */
    private boolean updatePointsGoods(PromotionMessage promotionMessage, String esPromotionKey, PromotionTypeEnum promotionTypeEnum){
        boolean result;
        PointsGoodsVO pointsGoodsVO = this.mongoTemplate.findById(promotionMessage.getPromotionId(), PointsGoodsVO.class);
        if (pointsGoodsVO == null) {
            this.throwPromotionException(promotionTypeEnum, promotionMessage.getPromotionId(), promotionMessage.getPromotionStatus());
            return false;
        }
        pointsGoodsVO.setPromotionStatus(promotionMessage.getPromotionStatus());
        result = this.pointsGoodsService.update(promotionMessage.updateWrapper());
        PointsGoods pointsGoods = JSONUtil.toBean(JSONUtil.toJsonStr(pointsGoodsVO), PointsGoods.class);
        this.goodsIndexService.updateEsGoodsIndex(pointsGoodsVO.getSkuId(), pointsGoods, esPromotionKey, null);
        this.mongoTemplate.save(pointsGoodsVO);
        return result;
    }

    /**
     * 修改优惠券活动状态
     * @param promotionMessage 信息队列传输促销信息实体
     * @param promotionTypeEnum 促销分类枚举
     * @return 修改结果
     */
    private boolean updateCouponActivity(PromotionMessage promotionMessage, PromotionTypeEnum promotionTypeEnum){
        boolean result;
        CouponActivityVO couponActivityVO = this.mongoTemplate.findById(promotionMessage.getPromotionId(), CouponActivityVO.class);
        if (couponActivityVO == null) {
            this.throwPromotionException(promotionTypeEnum, promotionMessage.getPromotionId(), promotionMessage.getPromotionStatus());
            return false;
        }
        couponActivityVO.setPromotionStatus(promotionMessage.getPromotionStatus());
        result = this.couponActivityService.update(promotionMessage.updateWrapper());
        this.mongoTemplate.save(couponActivityVO);
        return result;
    }

    /**
     * 更新促销商品信息
     *
     * @param promotionId 促销活动ID
     * @param promotionStatus 活动状态
     */
    private void updatePromotionGoods(String promotionId, String promotionStatus) {
        LambdaUpdateWrapper<PromotionGoods> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(PromotionGoods::getPromotionId, promotionId).set(PromotionGoods::getPromotionStatus, promotionStatus);
        this.promotionGoodsService.update(updateWrapper);
    }


    /**
     * 抛出促销异常
     *
     * @param type   促销类型
     * @param id     促销编号
     * @param status 促销状态
     */
    private void throwPromotionException(PromotionTypeEnum type, String id, String status) {
        log.error("当前" + type.name() + "活动ID为[" + id + "] 不存在，更改活动状态至[ " + status + " ]失败！");
        throw new ServiceException("当前活动已停止");
    }
}