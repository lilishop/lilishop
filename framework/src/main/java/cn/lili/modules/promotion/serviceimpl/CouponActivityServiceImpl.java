package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.CouponActivity;
import cn.lili.modules.promotion.entity.dos.CouponActivityItem;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dto.CouponActivityDTO;
import cn.lili.modules.promotion.entity.dto.CouponActivityTrigger;
import cn.lili.modules.promotion.entity.enums.*;
import cn.lili.modules.promotion.entity.vos.CouponActivityItemVO;
import cn.lili.modules.promotion.entity.vos.CouponActivityVO;
import cn.lili.modules.promotion.mapper.CouponActivityMapper;
import cn.lili.modules.promotion.service.*;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.trigger.enums.DelayTypeEnums;
import cn.lili.trigger.interfaces.TimeTrigger;
import cn.lili.trigger.message.CouponActivityMessage;
import cn.lili.trigger.model.TimeExecuteConstant;
import cn.lili.trigger.model.TimeTriggerMsg;
import cn.lili.trigger.util.DelayQueueTools;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 优惠券活动业务层实现
 *
 * @author Bulbasaur
 * @since 2021/5/20 6:10 下午
 */
@Service
@Slf4j
public class CouponActivityServiceImpl extends AbstractPromotionsServiceImpl<CouponActivityMapper, CouponActivity> implements CouponActivityService {

    @Autowired
    private CouponService couponService;
    @Autowired
    private MemberCouponService memberCouponService;
    @Autowired
    private CouponActivityItemService couponActivityItemService;


    @Autowired
    private MemberCouponSignService memberCouponSignService;

    @Autowired
    private MemberService memberService;


    @Autowired
    private Cache<List<CouponActivityVO>> cache;
    /**
     * 延时任务
     */
    @Autowired
    private TimeTrigger timeTrigger;
    /**
     * RocketMQ
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Override
    public CouponActivityVO getCouponActivityVO(String couponActivityId) {
        CouponActivity couponActivity = this.getById(couponActivityId);
        return new CouponActivityVO(couponActivity, couponActivityItemService.getCouponActivityItemListVO(couponActivityId));
    }


    @Override
    public void specify(CouponActivity couponActivity) {

        //如果开始时间为空，则表示活动关闭
        if (couponActivity.getStartTime() == null) {
            return;
        }

        TimeTriggerMsg timeTriggerMsg = new TimeTriggerMsg(TimeExecuteConstant.COUPON_ACTIVITY_EXECUTOR,
                couponActivity.getStartTime().getTime(),
                CouponActivityMessage.builder().couponActivityId(couponActivity.getId()).build(),
                DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.COUPON_ACTIVITY, couponActivity.getId()),
                rocketmqCustomProperties.getPromotionTopic());
        //发送促销活动开始的延时任务
        timeTrigger.addDelay(timeTriggerMsg);
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    public void specifyCoupon(String couponActivityId) {

        //获取优惠券活动
        CouponActivity couponActivity = this.getById(couponActivityId);
        //获取活动优惠券发送范围
        List<Map<String, Object>> member = this.getMemberList(couponActivity);

        //如果指定会员发券，则当下直接进行发送，如果是全体会员发券，则变更为用户登录首页进行请求发券
        //PS:即不主动发券，需要用户在活动时间内登录自动领取优惠券，类似美团、饿了么 的发放方式
        if (couponActivity.getActivityScope().equals(CouponActivitySendTypeEnum.DESIGNATED.name())) {
            //会员拆成多个小组进行发送
            List<List<Map<String, Object>>> memberGroup = new ArrayList<>();

            //循环分组
            for (int i = 0; i < (member.size() / 100 + (member.size() % 100 == 0 ? 0 : 1)); i++) {
                int endPoint = Math.min((100 + (i * 100)), member.size());
                memberGroup.add(member.subList((i * 100), endPoint));
            }

            //优惠优惠券活动的优惠券列表
            List<CouponActivityItem> couponActivityItems = couponActivityItemService.getCouponActivityList(couponActivity.getId());
            //发送优惠券
            for (List<Map<String, Object>> memberList : memberGroup) {
                sendCoupon(memberList, couponActivityItems);
            }
        }
    }

    /**
     * 初始化促销字段
     *
     * @param promotions 促销实体
     */
    @Override
    public void initPromotion(CouponActivity promotions) {
        super.initPromotion(promotions);
        if (promotions instanceof CouponActivityDTO) {
            CouponActivityDTO couponActivityDTO = (CouponActivityDTO) promotions;
            //如果有会员，则写入会员信息
            if (couponActivityDTO.getMemberDTOS() != null && !couponActivityDTO.getMemberDTOS().isEmpty()) {
                couponActivityDTO.setActivityScopeInfo(JSONUtil.toJsonStr(couponActivityDTO.getMemberDTOS()));
            }
        }
    }

    /**
     * 检查优惠券活动参数
     *
     * @param couponActivity 优惠券活动实体
     */
    @Override
    public void checkPromotions(CouponActivity couponActivity) {
        if (couponActivity instanceof CouponActivityDTO) {
            CouponActivityDTO couponActivityDTO = (CouponActivityDTO) couponActivity;
            //指定会员判定
            if (couponActivity.getActivityScope().equals(CouponActivitySendTypeEnum.DESIGNATED.name()) && couponActivityDTO.getMemberDTOS().isEmpty()) {
                throw new ServiceException(ResultCode.COUPON_ACTIVITY_MEMBER_ERROR);
            }
            // 检查优惠券
            this.checkCouponActivityItem(couponActivityDTO.getCouponActivityItems());
        }

    }

    /**
     * 更新优惠券活动商品信息
     *
     * @param couponActivity 优惠券活动实体
     * @return 是否更新成功
     */
    @Override
    @Transactional(rollbackFor = {Exception.class})
    public boolean updatePromotionsGoods(CouponActivity couponActivity) {
        boolean result = super.updatePromotionsGoods(couponActivity);
        if (couponActivity instanceof CouponActivityDTO && !PromotionsStatusEnum.CLOSE.name().equals(couponActivity.getPromotionStatus()) && PromotionsScopeTypeEnum.PORTION_GOODS.name().equals(couponActivity.getScopeType())) {
            CouponActivityDTO couponActivityDTO = (CouponActivityDTO) couponActivity;
            //创建优惠券活动子列表
            for (CouponActivityItem couponActivityItem : couponActivityDTO.getCouponActivityItems()) {
                couponActivityItem.setActivityId(couponActivityDTO.getId());
            }
            // 更新优惠券活动项信息
            result = couponActivityItemService.saveBatch(couponActivityDTO.getCouponActivityItems());
        }
        return result;
    }

    /**
     * 更新优惠券活动信息到商品索引
     *
     * @param couponActivity 促销实体
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateEsGoodsIndex(CouponActivity couponActivity) {


        switch (CouponActivityTypeEnum.valueOf(couponActivity.getCouponActivityType())) {

            // 精准发券 则立即发放
            case SPECIFY:
                this.specify(couponActivity);
                this.resetCache(couponActivity.getCouponActivityType());
                break;
            //其他活动则是缓存模块，根据缓存中的优惠券活动信息来确认发放优惠券测略
            case INVITE_NEW:
            case AUTO_COUPON:
            case REGISTERED:
                this.resetCache(couponActivity.getCouponActivityType());
                break;
        }

    }

    @Override
    public List<MemberCoupon> trigger(CouponActivityTrigger couponActivityTrigger) {
        //获取当前正在进行的优惠券活动
        List<CouponActivityVO> couponActivities = currentCouponActivity(couponActivityTrigger.getCouponActivityTypeEnum().name());


        /**
         * 自动发送优惠券则需要补足日志
         */
        if (couponActivityTrigger.getCouponActivityTypeEnum().equals(CouponActivityTypeEnum.AUTO_COUPON) || couponActivityTrigger.getCouponActivityTypeEnum().equals(CouponActivityTypeEnum.SPECIFY)) {
            couponActivities = memberCouponSignService.receiveCoupon(couponActivities);
        }


        //优惠券发放列表
        log.info("当前用户的优惠券活动信息:{}", couponActivityTrigger);
        log.info("当前进行的优惠券活动:{}", couponActivities);
        List<CouponActivityItemVO> couponActivityItemVOS = new ArrayList<>();

        //准备发放优惠券活动的列表
        couponActivities.forEach(item -> couponActivityItemVOS.addAll(item.getCouponActivityItems()));

        AuthUser authUser = new AuthUser();
        authUser.setId(couponActivityTrigger.getUserId());
        authUser.setNickName(couponActivityTrigger.getNickName());


        return this.sendCoupon(authUser, couponActivityItemVOS);
    }


    /**
     * 当前促销类型
     *
     * @return 当前促销类型
     */
    @Override
    public PromotionTypeEnum getPromotionType() {
        return PromotionTypeEnum.COUPON_ACTIVITY;
    }

    /**
     * 缓存key生成策略
     *
     * @param couponActivityType 优惠券活动类型
     * @return 缓存key
     */
    private String cacheKey(String couponActivityType) {
        return CachePrefix.CURRENT_COUPON_ACTIVITY.getPrefix() + couponActivityType;
    }

    /**
     * 当前进行的活动
     *
     * @return 当前进行的活动列表
     */
    private List<CouponActivityVO> currentCouponActivity() {
        return currentCouponActivity(CouponActivityTypeEnum.AUTO_COUPON.name());
    }

    /**
     * 当前进行的活动
     *
     * @return 当前进行的活动列表
     */
    private List<CouponActivityVO> currentCouponActivity(String couponActivityTypeEnum) {
        //获取缓存中的活动
        List<CouponActivityVO> couponActivityList = cache.get(cacheKey(couponActivityTypeEnum));
        if (couponActivityList == null) {
            return ongoingActivities(resetCache(couponActivityTypeEnum));
        }
        return ongoingActivities(couponActivityList);
    }

    /**
     * 从生效的活动优惠券中，过滤出正在进行的活动列表
     *
     * @param activityVOS
     * @return
     */
    private List<CouponActivityVO> ongoingActivities(List<CouponActivityVO> activityVOS) {
        if (activityVOS == null || activityVOS.isEmpty()) {
            return new ArrayList<>();
        }
        return activityVOS.stream().filter(item -> {
            return item.getPromotionStatus().equals(PromotionsStatusEnum.START.name());
        }).collect(Collectors.toList());
    }

    /**
     * 重写缓存中的活动优惠券信息
     */
    private List<CouponActivityVO> resetCache(String couponActivityType) {

        LambdaQueryWrapper<CouponActivity> lambdaQueryWrapper = new LambdaQueryWrapper<>();

        //如果结束时间大于当前时间，则表示有效
        lambdaQueryWrapper.gt(CouponActivity::getEndTime, new Date());
        //发送策略
        lambdaQueryWrapper.eq(CouponActivity::getActivityScope, CouponActivitySendTypeEnum.ALL);
        //活动类型
        lambdaQueryWrapper.eq(CouponActivity::getCouponActivityType, couponActivityType);

        //查询出结果，缓存后返回
        List<CouponActivity> couponActivities = list(lambdaQueryWrapper);

        List<CouponActivityVO> couponActivityVOS = new ArrayList<>();
        for (CouponActivity couponActivity : couponActivities) {
            couponActivityVOS.add(new CouponActivityVO(couponActivity, couponActivityItemService.getCouponActivityItemListVO(couponActivity.getId())));
        }

        cache.put(cacheKey(couponActivityType), couponActivityVOS);
        return couponActivityVOS;
    }

    /**
     * 定向发送优惠券
     *
     * @param memberList          用户列表
     * @param couponActivityItems 优惠券列表
     */
    private void sendCoupon(List<Map<String, Object>> memberList, List<CouponActivityItem> couponActivityItems) {

        for (Map<String, Object> map : memberList) {
            AuthUser authUser = new AuthUser();
            authUser.setId(map.get("id").toString());
            authUser.setNickName(map.get("nick_name").toString());

            sendCoupon(authUser, couponActivityItems);

        }
    }

    /**
     * 给当前用户发送优惠券
     * 1.循环优惠券列表
     * 2.判断优惠券每个会员发送数量
     * 3.记录优惠券发送数量
     *
     * @param authUser            发送目标用户
     * @param couponActivityItems 优惠券列表
     */
    private List<MemberCoupon> sendCoupon(AuthUser authUser, List<? extends CouponActivityItem> couponActivityItems) {

        //最终优惠券列表
        List<MemberCoupon> finalCoupons = new ArrayList<>();

        //循环优惠券赠送列表
        for (CouponActivityItem couponActivityItem : couponActivityItems) {
            //获取优惠券
            Coupon coupon = couponService.getById(couponActivityItem.getCouponId());
            //判断优惠券是否存在
            if (coupon != null) {
                //循环优惠券的领取数量
                int activitySendNum = couponActivityItem.getNum();

                List<MemberCoupon> memberCouponList = new ArrayList<>();
                for (int i = 1; i <= activitySendNum; i++) {
                    MemberCoupon memberCoupon = new MemberCoupon(coupon);
                    memberCoupon.setMemberId(authUser.getId());
                    memberCoupon.setMemberName(authUser.getNickName());
                    memberCoupon.setMemberCouponStatus(MemberCouponStatusEnum.NEW.name());
                    memberCoupon.setPlatformFlag(PromotionTools.PLATFORM_ID.equals(coupon.getStoreId()));
                    memberCouponList.add(memberCoupon);
                }

                finalCoupons.addAll(memberCouponList);
                //批量添加优惠券
                memberCouponService.saveBatch(memberCouponList);
                //添加优惠券已领取数量
                couponService.receiveCoupon(couponActivityItem.getCouponId(), memberCouponList.size());
            } else {
                log.error("赠送优惠券失败,当前优惠券不存在:" + couponActivityItem.getCouponId());
            }
        }
        if (finalCoupons.isEmpty()) {
            return new ArrayList<>();
        }
        return finalCoupons;

    }

    /**
     * 获取优惠券的范围范围
     * 此方法用于精准发券
     *
     * @param couponActivity 优惠券活动
     * @return 获取优惠券的会员列表
     */
    private List<Map<String, Object>> getMemberList(CouponActivity couponActivity) {
        //判断优惠券的发送范围，获取会员列表

        List<String> ids = new ArrayList<>();
        String scopeInfo = couponActivity.getActivityScopeInfo();
        try {
            JSONArray array = JSONUtil.parseArray(scopeInfo);
            ids = array.toList(Map.class).stream().map(i -> i.get("id").toString()).collect(Collectors.toList());
        } catch (Exception ignore) {
            // 非数组或格式错误时忽略，保持 ids 为空列表
        }
        return memberService.listFieldsByMemberIds("id,nick_name", ids);
    }

    /**
     * 检查优惠券
     *
     * @param couponActivityItems 优惠券列表
     */
    private void checkCouponActivityItem(List<CouponActivityItem> couponActivityItems) {
        //优惠券数量判定
        if (couponActivityItems.isEmpty()) {
            throw new ServiceException(ResultCode.COUPON_ACTIVITY_ITEM_ERROR);
        } else if (couponActivityItems.size() > 10) {
            throw new ServiceException(ResultCode.COUPON_ACTIVITY_ITEM_MUST_NUM_ERROR);
        } else {
            for (CouponActivityItem item : couponActivityItems) {
                if (item.getNum() == null || item.getNum() <= 0) {
                    throw new ServiceException(ResultCode.COUPON_ACTIVITY_ITEM_NUM_ERROR);
                }
                if (item.getNum() > 2) {
                    throw new ServiceException(ResultCode.COUPON_ACTIVITY_ITEM_NUM_MAX_VALUE_2);
                }
            }
        }
    }
}
