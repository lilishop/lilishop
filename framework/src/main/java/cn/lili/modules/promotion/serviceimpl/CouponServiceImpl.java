package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.delayqueue.DelayQueueTools;
import cn.lili.common.delayqueue.DelayQueueType;
import cn.lili.common.delayqueue.PromotionMessage;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.trigger.interfaces.TimeTrigger;
import cn.lili.common.trigger.model.TimeExecuteConstant;
import cn.lili.common.trigger.model.TimeTriggerMsg;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.enums.*;
import cn.lili.modules.promotion.entity.vos.CouponSearchParams;
import cn.lili.modules.promotion.entity.vos.CouponVO;
import cn.lili.modules.promotion.mapper.CouponMapper;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.FullDiscountService;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 优惠券活动业务层实现
 *
 * @author Chopper
 * @date 2020/8/21
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class CouponServiceImpl extends ServiceImpl<CouponMapper, Coupon> implements CouponService {

    //延时任务
    @Autowired
    private TimeTrigger timeTrigger;
    //Mongo
    @Autowired
    private MongoTemplate mongoTemplate;
    //规格商品
    @Autowired
    private GoodsSkuService goodsSkuService;
    //Rocketmq
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    //促销商品
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    //会员优惠券
    @Autowired
    private MemberCouponService memberCouponService;
    //满额活动
    @Autowired
    private FullDiscountService fullDiscountService;

    @Override
    public CouponVO add(CouponVO coupon) {
        // 检查参数
        this.checkParam(coupon);
        coupon.setUsedNum(0);
        coupon.setReceivedNum(0);
        // 保存到MYSQL中
        this.save(coupon);
        // 如果优惠券类型为部分商品则将促销活动商品更新
        this.updateScopePromotionGoods(coupon);
        // 保存到MONGO中
        this.mongoTemplate.save(coupon);
        // 如果是动态时间优惠券不走延时任务
        if (coupon.getRangeDayType().equals(CouponRangeDayEnum.FIXEDTIME.name())) {
            PromotionMessage promotionMessage = new PromotionMessage(coupon.getId(), PromotionTypeEnum.COUPON.name(), PromotionStatusEnum.START.name(), coupon.getStartTime(), coupon.getEndTime());
            TimeTriggerMsg timeTriggerMsg = new TimeTriggerMsg(TimeExecuteConstant.PROMOTION_EXECUTOR,
                    coupon.getStartTime().getTime(),
                    promotionMessage,
                    DelayQueueTools.wrapperUniqueKey(DelayQueueType.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                    rocketmqCustomProperties.getPromotionTopic());
            // 发送促销活动开始的延时任务
            this.timeTrigger.addDelay(timeTriggerMsg, DateUtil.getDelayTime(coupon.getStartTime().getTime()));
        }
        return coupon;
    }

    @Override
    public CouponVO updateCoupon(CouponVO couponVO) {
        CouponVO coupon = checkStatus(couponVO.getId());
        // 检查参数
        this.checkParam(couponVO);
        // 更新到MYSQL中
        this.updateById(couponVO);
        // 如果优惠券类型为部分商品则将促销活动商品更新
        this.updateScopePromotionGoods(couponVO);
        // 保存到MONGO中
        this.mongoTemplate.save(couponVO);
        // 如果是动态时间优惠券不走延时任务
        if (coupon.getRangeDayType().equals(CouponRangeDayEnum.FIXEDTIME.name())) {
            // 更新延时任务
            PromotionMessage promotionMessage = new PromotionMessage(couponVO.getId(), PromotionTypeEnum.COUPON.name(), PromotionStatusEnum.START.name(), coupon.getStartTime(), coupon.getEndTime());
            this.timeTrigger.edit(TimeExecuteConstant.PROMOTION_EXECUTOR,
                    promotionMessage,
                    coupon.getStartTime().getTime(), couponVO.getStartTime().getTime(),
                    DelayQueueTools.wrapperUniqueKey(DelayQueueType.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                    DateUtil.getDelayTime(couponVO.getStartTime().getTime()),
                    rocketmqCustomProperties.getPromotionTopic());
        }
        return couponVO;
    }

    @Override
    public boolean updateCouponStatus(List<String> couponId, PromotionStatusEnum promotionStatus) {
        Query query = new Query();
        query.addCriteria(Criteria.where("id").in(couponId));
        List<CouponVO> couponVOS = this.mongoTemplate.find(query, CouponVO.class);
        couponVOS = couponVOS.parallelStream().filter(i -> Boolean.FALSE.equals(i.getDeleteFlag())).collect(Collectors.toList());
        if (couponVOS.isEmpty()) {
            throw new ServiceException("优惠券不存在");
        }
        for (CouponVO couponVO : couponVOS) {
            if (promotionStatus.name().equals(PromotionStatusEnum.START.name())) {
                this.checkParam(couponVO);
            }
            couponVO.setPromotionStatus(promotionStatus.name());
            this.updateById(couponVO);
            this.mongoTemplate.save(couponVO);
            // 如果是动态时间优惠券不走延时任务
            if (couponVO.getRangeDayType().equals(CouponRangeDayEnum.FIXEDTIME.name())) {
                if (promotionStatus.name().equals(PromotionStatusEnum.START.name())) {
                    PromotionMessage promotionMessage = new PromotionMessage(couponVO.getId(), PromotionTypeEnum.COUPON.name(), PromotionStatusEnum.START.name(), couponVO.getStartTime(), couponVO.getEndTime());
                    // 更新延时任务
                    this.timeTrigger.edit(TimeExecuteConstant.PROMOTION_EXECUTOR,
                            promotionMessage,
                            couponVO.getStartTime().getTime(), couponVO.getStartTime().getTime(),
                            DelayQueueTools.wrapperUniqueKey(DelayQueueType.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                            DateUtil.getDelayTime(couponVO.getStartTime().getTime()),
                            rocketmqCustomProperties.getPromotionTopic());
                }
            }
        }
        return true;
    }

    @Override
    public boolean deleteCoupon(String id) {
        CouponVO couponVO = checkStatus(id);
        LambdaUpdateWrapper<Coupon> couponUpdateWrapper = new LambdaUpdateWrapper<Coupon>().eq(Coupon::getId, id).set(Coupon::getPromotionStatus, PromotionStatusEnum.CLOSE.name()).set(Coupon::getDeleteFlag, true);
        // 更新优惠券状态为关闭，标示删除标志
        boolean result = this.update(couponUpdateWrapper);
        LambdaUpdateWrapper<PromotionGoods> updateWrapper = new LambdaUpdateWrapper<PromotionGoods>().eq(PromotionGoods::getPromotionId, id).set(PromotionGoods::getPromotionStatus, PromotionStatusEnum.CLOSE.name()).set(PromotionGoods::getDeleteFlag, true);
        // 更新促销商品记录信息为删除
        this.promotionGoodsService.update(updateWrapper);
        LambdaUpdateWrapper<MemberCoupon> memberCouponLambdaUpdateWrapper = new LambdaUpdateWrapper<MemberCoupon>().eq(MemberCoupon::getCouponId, id).set(MemberCoupon::getMemberCouponStatus, MemberCouponStatusEnum.CLOSED.name());
        memberCouponService.update(memberCouponLambdaUpdateWrapper);
        this.mongoTemplate.remove(new Query().addCriteria(Criteria.where("id").is(id)), CouponVO.class);
        this.timeTrigger.delete(TimeExecuteConstant.PROMOTION_EXECUTOR,
                couponVO.getStartTime().getTime(),
                DelayQueueTools.wrapperUniqueKey(DelayQueueType.PROMOTION, (PromotionTypeEnum.COUPON.name() + couponVO.getId())),
                rocketmqCustomProperties.getPromotionTopic());
        return result;
    }

    @Override
    public IPage<CouponVO> getCouponsByPageFromMongo(CouponSearchParams param, PageVO page) {
        Query query = param.mongoQuery();
        IPage<CouponVO> coupons = new Page<>();
        if (page != null) {
            page.setNotConvert(true);
            PromotionTools.mongoQueryPageParam(query, page);
            coupons.setSize(page.getPageSize());
            coupons.setCurrent(page.getPageNumber());
        }
        List<CouponVO> couponVOList = mongoTemplate.find(query, CouponVO.class);
        coupons.setRecords(couponVOList);
        coupons.setTotal(mongoTemplate.count(query, CouponVO.class));
        return coupons;
    }

    /**
     * 根据查询条件从mongo中获取优惠券信息列表
     *
     * @param param 查询参数
     * @param page  分页参数
     * @return 优惠券信息列表
     */
    @Override
    public IPage<CouponVO> getCanReceiveCoupons(CouponSearchParams param, PageVO page) {
        Query query = param.mongoQuery();
        query.addCriteria(Criteria.where("promotionStatus").is(PromotionStatusEnum.START.name()));
        query.addCriteria(Criteria.where("endTime").gte(new Date()));
        IPage<CouponVO> coupons = new Page<>();
        if (page != null) {
            page.setNotConvert(true);
            PromotionTools.mongoQueryPageParam(query, page);
            coupons.setSize(page.getPageSize());
            coupons.setCurrent(page.getPageNumber());
        }
        List<CouponVO> couponVOList = mongoTemplate.find(query, CouponVO.class);
        coupons.setRecords(couponVOList);
        coupons.setTotal(mongoTemplate.count(query, CouponVO.class));
        return coupons;
    }

    @Override
    public CouponVO getCouponDetailFromMongo(String id) {
        return mongoTemplate.findById(id, CouponVO.class);
    }

    @Override
    public IPage<Coupon> getCouponsByPage(CouponSearchParams param, PageVO page) {
        QueryWrapper<Coupon> queryWrapper = param.wrapper();
        return page(PageUtil.initPage(page), queryWrapper);
    }

    /**
     * 领取优惠券
     *
     * @param couponId   优惠券id
     * @param receiveNum 领取数量
     */
    @Override
    public void receiveCoupon(String couponId, Integer receiveNum) {
        CouponVO couponVO = checkStatus(couponId);
        couponVO.setReceivedNum(couponVO.getReceivedNum() + receiveNum);
        LambdaUpdateWrapper<Coupon> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(Coupon::getId, couponId);
        updateWrapper.set(Coupon::getReceivedNum, couponVO.getReceivedNum());
        this.update(updateWrapper);
        this.mongoTemplate.save(couponVO);
    }

    /**
     * 使用优惠券
     *
     * @param couponId 优惠券id
     * @param usedNum  使用数量
     */
    @Override
    public void usedCoupon(String couponId, Integer usedNum) {
        CouponVO couponVO = checkStatus(couponId);
        couponVO.setUsedNum(couponVO.getUsedNum() + usedNum);
        LambdaUpdateWrapper<Coupon> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(Coupon::getId, couponId);
        updateWrapper.set(Coupon::getUsedNum, couponVO.getUsedNum());
        this.update(updateWrapper);
        this.mongoTemplate.save(couponVO);
    }

    /**
     * 检查优惠券信息是否合法
     *
     * @param coupon 优惠券信息
     */
    private void checkParam(CouponVO coupon) {
        //判断优惠券领取数量限制
        if (coupon.getCouponLimitNum() < 0) {
            throw new ServiceException("领取限制数量不能为负数");
        }
        //领取数量不能超过发放数量
        if (coupon.getCouponLimitNum() > coupon.getPublishNum()) {
            throw new ServiceException("领取限制数量超出发行数量");
        }
        //判断优惠券面额
        if (coupon.getCouponType().equals(CouponTypeEnum.PRICE.name()) && coupon.getPrice() > coupon.getConsumeThreshold()) {
            throw new ServiceException("优惠券面额必须小于优惠券消费限额");
        } else if (coupon.getCouponType().equals(CouponTypeEnum.DISCOUNT.name()) && (coupon.getCouponDiscount() < 0 && coupon.getCouponDiscount() > 10)) {
            throw new ServiceException("优惠券折扣必须小于10且大于0");
        }
        //判断优惠券时间
        long nowTime = DateUtil.getDateline() * 1000;
        if (coupon.getRangeDayType().equals(CouponRangeDayEnum.FIXEDTIME.name())) {
            if (coupon.getStartTime().getTime() < nowTime && coupon.getEndTime().getTime() > nowTime) {
                throw new ServiceException("活动时间小于当前时间，不能进行编辑删除操作");
            }

            PromotionTools.checkPromotionTime(coupon.getStartTime().getTime(), coupon.getEndTime().getTime());
            //对状态的处理.如果未传递状态则需要 根据当前时间来确认优惠券状态
            this.promotionStatusEmpty(coupon);
        } else {
            //动态时间优惠券需设置有限期并为活动赠送优惠券
            if (coupon.getEffectiveDays() == null || coupon.getGetType().equals(CouponGetEnum.FREE.name())) {
                throw new ServiceException("活动赠送优惠券需设置有限期");
            }
        }
        this.checkCouponScope(coupon);
    }

    /**
     * 检查优惠券范围
     *
     * @param coupon 检查的优惠券对象
     */
    private void checkCouponScope(CouponVO coupon) {
        if (coupon.getScopeType().equals(CouponScopeTypeEnum.PORTION_GOODS.name()) && (coupon.getPromotionGoodsList() == null || coupon.getPromotionGoodsList().isEmpty())) {
            throw new ServiceException("当前关联范围类型为指定商品时，商品列表不能为空");
        } else if (coupon.getScopeType().equals(CouponScopeTypeEnum.PORTION_GOODS.name()) && CharSequenceUtil.isEmpty(coupon.getScopeId())) {
            throw new ServiceException("当前关联范围类型为指定商品时，范围关联的id不能为空");
        } else if (coupon.getScopeType().equals(CouponScopeTypeEnum.PORTION_GOODS_CATEGORY.name()) && CharSequenceUtil.isEmpty(coupon.getScopeId())) {
            throw new ServiceException("当前关联范围类型为部分商品分类时，范围关联的id不能为空");
        } else if (coupon.getScopeType().equals(CouponScopeTypeEnum.PORTION_SHOP_CATEGORY.name()) && CharSequenceUtil.isEmpty(coupon.getScopeId())) {
            throw new ServiceException("当前关联范围类型为部分店铺分类时，范围关联的id不能为空");
        }

        if (coupon.getScopeType().equals(CouponScopeTypeEnum.PORTION_GOODS.name())) {
            String[] split = coupon.getScopeId().split(",");
            if (split.length <= 0) {
                throw new ServiceException("指定商品范围关联id无效！");
            }
            for (String id : split) {
                GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(id);
                if (goodsSku == null) {
                    throw new ServiceException("商品已下架");
                }
            }
        }
    }

    /**
     * 对状态的处理.如果未传递状态则需要 根据当前时间来确认优惠券状态
     *
     * @param coupon 优惠券参数
     */
    private void promotionStatusEmpty(CouponVO coupon) {
        if (StringUtils.isEmpty(coupon.getPromotionStatus())) {
            //格式时间
            long startTme = coupon.getStartTime().getTime() / 1000;
            long endTime = coupon.getEndTime().getTime() / 1000;
            //校验时间确定当前优惠券有效期
            long currentTime = DateUtil.getDateline();
            //如果未到时间点则为新建
            if (startTme > currentTime) {
                coupon.setPromotionStatus(PromotionStatusEnum.NEW.name());
            }
            //如果超过结束时间则为结束
            if (endTime < currentTime) {
                coupon.setPromotionStatus(PromotionStatusEnum.END.name());
            }
            //如果在使用时间内 则是开始状态
            if (startTme <= currentTime && endTime > currentTime) {
                coupon.setPromotionStatus(PromotionStatusEnum.START.name());
            }
        }
    }

    /**
     * 检查优惠券状态是否可进行编辑删除
     *
     * @param id 优惠券id
     * @return 优惠券信息
     */
    private CouponVO checkStatus(String id) {
        CouponVO coupon = this.mongoTemplate.findById(id, CouponVO.class);
        if (coupon == null) {
            throw new ServiceException("当前优惠券活动不存在");
        }
        LambdaQueryWrapper<FullDiscount> queryWrapper = new LambdaQueryWrapper<FullDiscount>().eq(FullDiscount::getIsCoupon, true).eq(FullDiscount::getCouponId, id);
        FullDiscount fullDiscount = fullDiscountService.getOne(queryWrapper);
        if (fullDiscount != null) {
            throw new ServiceException("当前优惠券参与了促销活动 " + fullDiscount.getTitle() + " 不能进行编辑删除操作");
        }
        return coupon;
    }

    private void updateScopePromotionGoods(CouponVO couponVO) {
        // 如果优惠券类型为部分商品则将促销活动更新至ES中
        if (CouponScopeTypeEnum.PORTION_GOODS.name().equals(couponVO.getScopeType()) && !couponVO.getPromotionGoodsList().isEmpty()) {
            PromotionTools.promotionGoodsInit(couponVO.getPromotionGoodsList(), couponVO, PromotionTypeEnum.COUPON);
        }
    }

}