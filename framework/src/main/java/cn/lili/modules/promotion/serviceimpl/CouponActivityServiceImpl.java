package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.CouponActivity;
import cn.lili.modules.promotion.entity.dos.CouponActivityItem;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dto.CouponActivityDTO;
import cn.lili.modules.promotion.entity.enums.*;
import cn.lili.modules.promotion.entity.vos.CouponActivityVO;
import cn.lili.modules.promotion.mapper.CouponActivityMapper;
import cn.lili.modules.promotion.service.CouponActivityItemService;
import cn.lili.modules.promotion.service.CouponActivityService;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.modules.promotion.tools.PromotionTools;
import groovy.util.logging.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 优惠券活动业务层实现
 *
 * @author Bulbasaur
 * @since 2021/5/20 6:10 下午
 */
@Slf4j
@Service
public class CouponActivityServiceImpl extends AbstractPromotionsServiceImpl<CouponActivityMapper, CouponActivity> implements CouponActivityService {

    @Autowired
    private CouponService couponService;
    @Autowired
    private MemberCouponService memberCouponService;
    @Autowired
    private CouponActivityItemService couponActivityItemService;
    @Autowired
    private MemberService memberService;

    @Override
    public CouponActivityVO getCouponActivityVO(String couponActivityId) {
        CouponActivity couponActivity = this.getById(couponActivityId);
        return new CouponActivityVO(couponActivity, couponActivityItemService.getCouponActivityItemListVO(couponActivityId));
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
        super.checkPromotions(couponActivity);

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
    public boolean updatePromotionsGoods(CouponActivity couponActivity) {
        boolean result = super.updatePromotionsGoods(couponActivity);
        if (couponActivity instanceof CouponActivityDTO
                && !PromotionsStatusEnum.CLOSE.name().equals(couponActivity.getPromotionStatus())
                && PromotionsScopeTypeEnum.PORTION_GOODS.name().equals(couponActivity.getScopeType())) {
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
    public void updateEsGoodsIndex(CouponActivity couponActivity) {
        //如果是精准发券，进行发送优惠券
        if (!PromotionsStatusEnum.CLOSE.name().equals(couponActivity.getPromotionStatus()) && couponActivity.getCouponActivityType().equals(CouponActivityTypeEnum.SPECIFY.name())) {
            this.specify(couponActivity.getId());
        }
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
            Coupon coupon = couponService.getById(couponActivityItem.getCouponId());
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
                        memberCoupon.setPlatformFlag(PromotionTools.PLATFORM_ID.equals(coupon.getStoreId()));
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
     * 获取优惠券的范围范围
     * 此方法用于精准发券
     *
     * @param couponActivity 优惠券活动
     * @return 获取优惠券的会员列表
     */
    private List<Map<String, Object>> getMemberList(CouponActivity couponActivity) {
        //判断优惠券的发送范围，获取会员列表
        if ("ALL".equals(couponActivity.getActivityScope())) {
            return this.memberService.listFieldsByMemberIds("id,nick_name", null);
        } else {
            List<String> ids = new ArrayList<>();
            if (JSONUtil.isJsonArray(couponActivity.getActivityScopeInfo())) {
                JSONArray array = JSONUtil.parseArray(couponActivity.getActivityScopeInfo());
                ids = array.toList(Map.class).stream().map(i -> i.get("id").toString()).collect(Collectors.toList());
            }
            return memberService.listFieldsByMemberIds("id,nick_name", ids);
        }
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
            }
        }
    }
}
