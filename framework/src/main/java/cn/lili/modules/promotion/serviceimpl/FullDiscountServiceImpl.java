package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.trigger.util.DelayQueueTools;
import cn.lili.common.trigger.enums.DelayTypeEnums;
import cn.lili.common.trigger.message.PromotionMessage;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.trigger.interfaces.TimeTrigger;
import cn.lili.common.trigger.model.TimeExecuteConstant;
import cn.lili.common.trigger.model.TimeTriggerMsg;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.vos.FullDiscountSearchParams;
import cn.lili.modules.promotion.mapper.FullDiscountMapper;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.FullDiscountService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
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

/**
 * 满优惠业务层实现
 *
 * @author Chopper
 * @date 2020/8/21
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class FullDiscountServiceImpl extends ServiceImpl<FullDiscountMapper, FullDiscount> implements FullDiscountService {

    private static final String SELLER_ID_COLUMN = "storeId";
    private static final String PROMOTION_STATUS_COLUMN = "promotionStatus";

    /**
     * 延时任务
     */
    @Autowired
    private TimeTrigger timeTrigger;
    /**
     * Mongo
     */
    @Autowired
    private MongoTemplate mongoTemplate;
    /**
     * Rocketmq
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
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
    public FullDiscountVO currentPromotion(String storeId) {
        Query query = this.getMongoQuery();
        query.addCriteria(Criteria.where(SELLER_ID_COLUMN).is(storeId));
        return mongoTemplate.findOne(query, FullDiscountVO.class);
    }

    @Override
    public List<FullDiscountVO> currentPromotion(List<String> storeId) {
        Query query = this.getMongoQuery();
        query.addCriteria(Criteria.where(SELLER_ID_COLUMN).in(storeId));
        return mongoTemplate.find(query, FullDiscountVO.class);
    }

    @Override
    public FullDiscount addFullDiscount(FullDiscountVO fullDiscountVO) {
        //验证是否是有效参数
        PromotionTools.paramValid(fullDiscountVO.getStartTime().getTime(), fullDiscountVO.getEndTime().getTime(), fullDiscountVO.getNumber(), fullDiscountVO.getPromotionGoodsList());
        //当前时间段是否存在同类活动
        this.checkSameActiveExist(fullDiscountVO.getStartTime(), fullDiscountVO.getEndTime(), fullDiscountVO.getStoreId(), null);
        //检查满减参数
        this.checkFullDiscount(fullDiscountVO);
        //保存到MYSQL中
        this.save(fullDiscountVO);
        if (fullDiscountVO.getPromotionGoodsList() != null) {
            List<PromotionGoods> promotionGoodsList = PromotionTools.promotionGoodsInit(fullDiscountVO.getPromotionGoodsList(), fullDiscountVO, PromotionTypeEnum.FULL_DISCOUNT);
            //促销活动商品更新
            this.promotionGoodsService.saveOrUpdateBatch(promotionGoodsList);
        }
        //保存到MONGO中
        this.mongoTemplate.save(fullDiscountVO);
        PromotionMessage promotionMessage = new PromotionMessage(fullDiscountVO.getId(), PromotionTypeEnum.FULL_DISCOUNT.name(),
                PromotionStatusEnum.START.name(),
                fullDiscountVO.getStartTime(), fullDiscountVO.getEndTime());

        TimeTriggerMsg timeTriggerMsg = new TimeTriggerMsg(TimeExecuteConstant.PROMOTION_EXECUTOR,
                fullDiscountVO.getStartTime().getTime(), promotionMessage,
                DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                rocketmqCustomProperties.getPromotionTopic());
        //发送促销活动开始的延时任务
        this.timeTrigger.addDelay(timeTriggerMsg);
        return fullDiscountVO;
    }

    @Override
    public IPage<FullDiscount> getFullDiscountByPageFromMysql(FullDiscountSearchParams searchParams, PageVO page) {
        QueryWrapper<FullDiscount> queryWrapper = searchParams.wrapper();
        return this.page(PageUtil.initPage(page), queryWrapper);
    }

    @Override
    public IPage<FullDiscountVO> getFullDiscountByPageFromMongo(FullDiscountSearchParams searchParams, PageVO page) {
        IPage<FullDiscountVO> fullDiscountPage = new Page<>();
        Query query = searchParams.mongoQuery();
        if (page != null) {
            PromotionTools.mongoQueryPageParam(query, page);
            fullDiscountPage.setCurrent(page.getPageNumber());
            fullDiscountPage.setSize(page.getPageSize());
        }
        List<FullDiscountVO> fullDiscountVOS = this.mongoTemplate.find(query, FullDiscountVO.class);
        fullDiscountPage.setRecords(fullDiscountVOS);
        fullDiscountPage.setTotal(this.mongoTemplate.count(query, FullDiscountVO.class));
        return fullDiscountPage;
    }

    @Override
    public FullDiscountVO modifyFullDiscount(FullDiscountVO fullDiscountVO) {
        //检查满优惠活动是否存在
        FullDiscountVO fullDiscount = this.checkFullDiscountExist(fullDiscountVO.getId());
        if (!fullDiscount.getPromotionStatus().equals(PromotionStatusEnum.NEW.name())) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_MODIFY_ERROR);
        }
        //检查活动是否已经开始
        PromotionTools.checkPromotionTime(fullDiscountVO.getStartTime().getTime(), fullDiscountVO.getEndTime().getTime());
        //检查满减参数
        this.checkFullDiscount(fullDiscountVO);
        //时间发生变化
        if (!fullDiscount.getStartTime().equals(fullDiscountVO.getStartTime()) && fullDiscount.getEndTime().equals(fullDiscountVO.getEndTime())) {
            //检查当前时间段是否存在同类活动
            this.checkSameActiveExist(fullDiscountVO.getStartTime(), fullDiscountVO.getEndTime(), fullDiscountVO.getStoreId(), fullDiscount.getId());

        }
        //更新到MYSQL中
        this.updateById(fullDiscountVO);
        if (fullDiscountVO.getPromotionGoodsList() != null) {
            //促销活动商品更新
            this.promotionGoodsService.updateBatchById(PromotionTools.promotionGoodsInit(fullDiscountVO.getPromotionGoodsList(), fullDiscountVO, PromotionTypeEnum.FULL_DISCOUNT));
        }
        //保存到MONGO中
        this.mongoTemplate.save(fullDiscountVO);
        PromotionMessage promotionMessage = new PromotionMessage(fullDiscountVO.getId(), PromotionTypeEnum.FULL_DISCOUNT.name(), PromotionStatusEnum.START.name(), fullDiscountVO.getStartTime(), fullDiscountVO.getEndTime());
        //发送更新延时任务
        this.timeTrigger.edit(TimeExecuteConstant.PROMOTION_EXECUTOR, promotionMessage,
                fullDiscount.getStartTime().getTime(), fullDiscountVO.getStartTime().getTime(),
                DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                DateUtil.getDelayTime(fullDiscountVO.getStartTime().getTime()),
                rocketmqCustomProperties.getPromotionTopic());
        return fullDiscountVO;
    }

    /**
     * 获取满优惠活动详情
     *
     * @param id 满优惠KID
     * @return 满优惠活动详情
     */
    @Override
    public FullDiscountVO getFullDiscount(String id) {
        return this.checkFullDiscountExist(id);
    }

    @Override
    public boolean deleteFullDiscount(String id) {
        FullDiscountVO fullDiscount = this.checkFullDiscountExist(id);
        //检查活动是否已经开始
        boolean result = this.removeById(id);
        this.mongoTemplate.remove(new Query().addCriteria(Criteria.where("id").is(id)), FullDiscountVO.class);
        if (fullDiscount.getPromotionGoodsList() != null && !fullDiscount.getPromotionGoodsList().isEmpty()) {
            this.promotionGoodsService.removePromotionGoods(fullDiscount.getPromotionGoodsList(), PromotionTypeEnum.FULL_DISCOUNT);
        }
        this.timeTrigger.delete(TimeExecuteConstant.PROMOTION_EXECUTOR, fullDiscount.getStartTime().getTime(),
                DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (PromotionTypeEnum.FULL_DISCOUNT.name() + fullDiscount.getId())),
                rocketmqCustomProperties.getPromotionTopic());
        return result;
    }

    /**
     * 检查满优惠活动是否存在
     *
     * @param id 满优惠活动id
     * @return 满优惠活动
     */
    private FullDiscountVO checkFullDiscountExist(String id) {
        FullDiscountVO fullDiscountVO = mongoTemplate.findById(id, FullDiscountVO.class);
        if (fullDiscountVO == null) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_NOT_EXIST_ERROR);
        }
        return fullDiscountVO;
    }

    /**
     * 检查满减参数
     *
     * @param fullDiscountVO 满减参数信息
     */
    private void checkFullDiscount(FullDiscountVO fullDiscountVO) {
        if (fullDiscountVO.getIsFullMinus() == null && fullDiscountVO.getIsCoupon() == null && fullDiscountVO.getIsGift() == null && fullDiscountVO.getIsPoint() == null && fullDiscountVO.getIsFullRate() == null) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_WAY_ERROR);
        }
        //如果优惠方式是满减
        if (Boolean.TRUE.equals(fullDiscountVO.getIsFullMinus())) {
            this.checkFullMinus(fullDiscountVO.getFullMinus(), fullDiscountVO.getFullMoney());
            fullDiscountVO.setTitle("满" + fullDiscountVO.getFullMoney() + " 减" + fullDiscountVO.getFullMinus());
        }
        //如果优惠方式是赠品
        if (Boolean.TRUE.equals(fullDiscountVO.getIsGift())) {
            //是否没有选择赠品
            boolean noGiftSelected = fullDiscountVO.getGiftId() == null;
            if (noGiftSelected) {
                throw new ServiceException(ResultCode.FULL_DISCOUNT_GIFT_ERROR);
            }
        }
        //如果优惠方式是赠优惠券
        if (Boolean.TRUE.equals(fullDiscountVO.getIsCoupon())) {
            this.checkCoupon(fullDiscountVO.getCouponId(), fullDiscountVO.getEndTime().getTime());
        }
        //如果优惠方式是折扣
        if (Boolean.TRUE.equals(fullDiscountVO.getIsFullRate())) {
            this.checkFullRate(fullDiscountVO.getFullRate());
            fullDiscountVO.setTitle("满" + fullDiscountVO.getFullMoney() + " 打" + fullDiscountVO.getFullRate() + "折");
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
        Integer sameNum = this.count(queryWrapper);
        if (sameNum > 0) {
            throw new ServiceException(ResultCode.PROMOTION_SAME_ACTIVE_EXIST);
        }
    }

    /**
     * 检查优惠券信息
     *
     * @param couponId 优惠券编号
     * @param endTime  活动结束时间
     */
    private void checkCoupon(String couponId, long endTime) {
        //是否没有选择优惠券
        boolean noCouponSelected = couponId == null;
        if (noCouponSelected) {
            throw new ServiceException(ResultCode.COUPON_NOT_EXIST);
        }
        Coupon coupon = this.couponService.getById(couponId);
        if (coupon.getEndTime().getTime() < endTime) {
            throw new ServiceException(ResultCode.FULL_DISCOUNT_COUPON_TIME_ERROR);
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

    /**
     * 通用有效的满优惠活动mongo查询
     *
     * @return mongo查询对象
     */
    private Query getMongoQuery() {
        Query query = new Query();
        Date now = new Date();
        query.addCriteria(Criteria.where(PROMOTION_STATUS_COLUMN).is(PromotionStatusEnum.START.name()));
        query.addCriteria(Criteria.where("startTime").lt(now));
        query.addCriteria(Criteria.where("endTime").gt(now));
        return query;
    }
}