package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.trigger.enums.DelayTypeEnums;
import cn.lili.common.trigger.interfaces.TimeTrigger;
import cn.lili.common.trigger.message.PromotionMessage;
import cn.lili.common.trigger.model.TimeExecuteConstant;
import cn.lili.common.trigger.model.TimeTriggerMsg;
import cn.lili.common.trigger.util.DelayQueueTools;
import cn.lili.common.utils.DateUtil;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.CouponActivity;
import cn.lili.modules.promotion.entity.dos.CouponActivityItem;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dto.CouponActivityDTO;
import cn.lili.modules.promotion.entity.enums.CouponActivitySendTypeEnum;
import cn.lili.modules.promotion.entity.enums.MemberCouponStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.vos.CouponActivityVO;
import cn.lili.modules.promotion.mapper.CouponActivityMapper;
import cn.lili.modules.promotion.service.CouponActivityItemService;
import cn.lili.modules.promotion.service.CouponActivityService;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import groovy.util.logging.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * 优惠券活动业务层实现
 *
 * @author Bulbasaur
 * @date: 2021/5/20 6:10 下午
 */
@Slf4j
@Service
public class CouponActivityServiceImpl extends ServiceImpl<CouponActivityMapper, CouponActivity> implements CouponActivityService {

    @Autowired
    private CouponService couponService;
    @Autowired
    private MemberCouponService memberCouponService;
    @Autowired
    private CouponActivityItemService couponActivityItemService;
    @Autowired
    private MemberService memberService;
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    @Autowired
    private TimeTrigger timeTrigger;

    @Override
    public CouponActivityDTO addCouponActivity(CouponActivityDTO couponActivityDTO) {
        //检测优惠券活动是否可以添加
        this.checkParam(couponActivityDTO);
        //如果有会员，则写入会员信息
        if (couponActivityDTO.getMemberDTOS() != null && couponActivityDTO.getMemberDTOS().size() != 0) {
            couponActivityDTO.setActivityScopeInfo(JSONUtil.toJsonStr(couponActivityDTO.getMemberDTOS()));
        }
        //添加优惠券活动
        this.save(couponActivityDTO);
        //添加优惠券活动优惠券
        this.addCouponActivityItems(couponActivityDTO);

        //创建优惠券活动延时任务
        PromotionMessage promotionMessage = new PromotionMessage(couponActivityDTO.getId(), PromotionTypeEnum.COUPON_ACTIVITY.name(), PromotionStatusEnum.START.name(), couponActivityDTO.getStartTime(), couponActivityDTO.getEndTime());
        TimeTriggerMsg timeTriggerMsg = new TimeTriggerMsg(TimeExecuteConstant.PROMOTION_EXECUTOR,
                couponActivityDTO.getStartTime().getTime(),
                promotionMessage,
                DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.PROMOTION, (promotionMessage.getPromotionType() + promotionMessage.getPromotionId())),
                rocketmqCustomProperties.getPromotionTopic());
        //发送促销活动开始的延时任务
        this.timeTrigger.addDelay(timeTriggerMsg);

        return couponActivityDTO;
    }

    @Override
    public CouponActivityDTO updateCouponActivity(CouponActivityDTO couponActivityDTO) {
        //检测优惠券活动是否可以修改
        this.checkParam(couponActivityDTO);
        //修改优惠券活动
        this.updateById(couponActivityDTO);
        //删除优惠券活动关联的优惠券
        couponActivityItemService.remove(new LambdaQueryWrapper<CouponActivityItem>()
                .eq(CouponActivityItem::getActivityId, couponActivityDTO.getId()));
        //重新添加优惠券活动关联优惠券
        this.addCouponActivityItems(couponActivityDTO);
        return couponActivityDTO;
    }

    @Override
    public CouponActivityVO getCouponActivityVO(String couponActivityId) {
        CouponActivity couponActivity = this.getById(couponActivityId);
        CouponActivityVO couponActivityVO = new CouponActivityVO(couponActivity, couponActivityItemService.getCouponActivityItemListVO(couponActivityId));
        return couponActivityVO;
    }

    @Override
    public void specify(String couponActivityId) {
        //获取优惠券
        CouponActivity couponActivity = this.getById(couponActivityId);
        //获取活动优惠券发送范围
        List<Map<String, Object>> member = this.getMemberList(couponActivity);

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

    @Override
    public void registered(List<CouponActivity> couponActivityList, Member member) {
        for (CouponActivity couponActivity : couponActivityList) {
            //获取会员信息
            List<Map<String, Object>> memberList = new ArrayList<>();
            Map<String, Object> map = new HashMap<>(2);
            map.put("id", member.getId());
            map.put("nick_name", member.getNickName());
            memberList.add(map);

            //优惠优惠券活动的优惠券列表
            List<CouponActivityItem> couponActivityItems = couponActivityItemService.getCouponActivityList(couponActivity.getId());

            //发送优惠券
            sendCoupon(memberList, couponActivityItems);
        }
    }

    @Override
    public boolean updateCouponActivityStatus(String id, PromotionStatusEnum promotionStatus) {
        CouponActivity couponActivity = this.getById(id);
        couponActivity.setPromotionStatus(promotionStatus.name());
        return this.updateById(couponActivity);
    }

    /**
     * 发送优惠券
     * 1.循环优惠券列表
     * 2.判断优惠券每个会员发送数量
     * 3.循环会员列表，发送优惠券
     * 4.记录优惠券发送数量
     *
     * @param memberList          用户列表
     * @param couponActivityItems 优惠券列表
     */
    private void sendCoupon(List<Map<String, Object>> memberList, List<CouponActivityItem> couponActivityItems) {

        for (CouponActivityItem couponActivityItem : couponActivityItems) {
            //获取优惠券
            Coupon coupon = couponService.getCouponDetailFromMongo(couponActivityItem.getCouponId());
            //判断优惠券是否存在
            if (coupon != null) {
                List<MemberCoupon> memberCouponList = new LinkedList<>();
                //循环优惠券的领取数量
                int j = couponActivityItem.getNum();
                for (int i = 1; i <= j; i++) {
                    //循环会员列表，添加优惠券
                    for (Map<String, Object> map : memberList) {
                        MemberCoupon memberCoupon = new MemberCoupon(coupon);
                        memberCoupon.setMemberId(map.get("id").toString());
                        memberCoupon.setMemberName(map.get("nick_name").toString());
                        memberCoupon.setMemberCouponStatus(MemberCouponStatusEnum.NEW.name());
                        memberCoupon.setIsPlatform("platform".equals(coupon.getStoreId()));
                        memberCouponList.add(memberCoupon);
                    }
                }
                //批量添加优惠券
                memberCouponService.saveBatch(memberCouponList);
                //添加优惠券已领取数量
                couponService.receiveCoupon(couponActivityItem.getCouponId(), memberCouponList.size() * couponActivityItem.getNum());
            } else {
                log.error("赠送优惠券失败,当前优惠券不存在:" + couponActivityItem.getCouponId());
            }
        }

    }

    /**
     * 检测优惠券活动参数
     *
     * @param couponActivity 优惠券活动
     */
    private void checkParam(CouponActivityDTO couponActivity) {

        //检测活动时间超过当前时间不能进行操作
        long nowTime = DateUtil.getDateline() * 1000;
        if (couponActivity.getStartTime().getTime() < nowTime && couponActivity.getEndTime().getTime() > nowTime) {
            throw new ServiceException(ResultCode.COUPON_ACTIVITY_START_TIME_ERROR);
        }
        //活动时间需超过当前时间
        PromotionTools.checkPromotionTime(couponActivity.getStartTime().getTime(), couponActivity.getEndTime().getTime());
        //指定会员判定
        if (couponActivity.getActivityScope().equals(CouponActivitySendTypeEnum.DESIGNATED.name())) {
            if (couponActivity.getMemberDTOS().size() == 0) {
                throw new ServiceException(ResultCode.COUPON_ACTIVITY_MEMBER_ERROR);
            }
        }
        //优惠券数量判定
        if (couponActivity.getCouponActivityItems().size() == 0) {
            throw new ServiceException(ResultCode.COUPON_ACTIVITY_ITEM_ERROR);
        } else if (couponActivity.getCouponActivityItems().size() > 10) {
            throw new ServiceException(ResultCode.COUPON_ACTIVITY_ITEM_MUST_NUM_ERROR);
        } else {
            for (CouponActivityItem item : couponActivity.getCouponActivityItems()) {
                if (item.getNum() == null || item.getNum() <= 0) {
                    throw new ServiceException(ResultCode.COUPON_ACTIVITY_ITEM_NUM_ERROR);
                }
            }
        }
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
        if ("ALL".equals(couponActivity.getActivityScope())) {
            return memberService.listMaps(new QueryWrapper<Member>()
                    .select("id,nick_name"));
        } else {
            return memberService.listMaps(new QueryWrapper<Member>()
                    .select("id,nick_name")
                    .in("id", couponActivity.getActivityScopeInfo()));
        }
    }

    /**
     * 添加优惠券活动关联优惠券
     *
     * @param couponActivityDTO 优惠券活动DTO
     */
    private void addCouponActivityItems(CouponActivityDTO couponActivityDTO) {
        //创建优惠券活动子列表
        for (CouponActivityItem couponActivityItem : couponActivityDTO.getCouponActivityItems()) {
            couponActivityItem.setActivityId(couponActivityDTO.getId());
        }
        couponActivityItemService.saveBatch(couponActivityDTO.getCouponActivityItems());
    }
}
