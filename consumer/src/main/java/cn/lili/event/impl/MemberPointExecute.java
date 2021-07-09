package cn.lili.event.impl;


import cn.lili.common.utils.CurrencyUtil;
import cn.lili.event.AfterSaleStatusChangeEvent;
import cn.lili.event.GoodsCommentCompleteEvent;
import cn.lili.event.MemberRegisterEvent;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.order.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.PointSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.google.gson.Gson;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 会员积分
 *
 * @author Bulbasaur
 * @date 2020-07-03 11:20
 */
@Service
public class MemberPointExecute implements MemberRegisterEvent, GoodsCommentCompleteEvent, OrderStatusChangeEvent, AfterSaleStatusChangeEvent {

    /**
     * 配置
     */
    @Autowired
    private SettingService settingService;
    /**
     * 会员
     */
    @Autowired
    private MemberService memberService;
    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;

    /**
     * 会员注册赠送积分
     *
     * @param member 会员
     */
    @Override
    public void memberRegister(Member member) {
        //获取积分设置
        PointSetting pointSetting = getPointSetting();
        //赠送会员积分
        memberService.updateMemberPoint(Long.valueOf(pointSetting.getRegister().longValue()), true, member.getId(), "会员注册，赠送积分" + pointSetting.getRegister() + "分");
    }

    /**
     * 会员评价赠送积分
     *
     * @param memberEvaluation 会员评价
     */
    @Override
    public void goodsComment(MemberEvaluation memberEvaluation) {
        //获取积分设置
        PointSetting pointSetting = getPointSetting();
        //赠送会员积分
        memberService.updateMemberPoint(Long.valueOf(pointSetting.getComment().longValue()), true, memberEvaluation.getMemberId(), "会员评价，赠送积分" + pointSetting.getComment() + "分");
    }

    /**
     * 非积分订单订单完成后赠送积分
     *
     * @param orderMessage 订单消息
     */
    @Override
    public void orderChange(OrderMessage orderMessage) {

        if (orderMessage.getNewStatus().equals(OrderStatusEnum.COMPLETED)) {
            //根据订单编号获取订单数据,如果为积分订单则跳回
            Order order = orderService.getBySn(orderMessage.getOrderSn());
            if (order.getOrderPromotionType().equals(OrderPromotionTypeEnum.POINT.name())) {
                return;
            }
            //获取积分设置
            PointSetting pointSetting = getPointSetting();
            //计算赠送积分数量
            Double point = CurrencyUtil.mul(pointSetting.getMoney(), order.getFlowPrice(), 0);
            //赠送会员积分
            memberService.updateMemberPoint(point.longValue(), true, order.getMemberId(), "会员下单，赠送积分" + point + "分");

        }
    }

    /**
     * 提交售后后扣除积分
     *
     * @param afterSale 售后
     */
    @Override
    public void afterSaleStatusChange(AfterSale afterSale) {
        if (afterSale.getServiceStatus().equals(AfterSaleStatusEnum.COMPLETE.name())) {
            //获取积分设置
            PointSetting pointSetting = getPointSetting();
            //计算扣除积分数量
            Double point = CurrencyUtil.mul(pointSetting.getMoney(), afterSale.getActualRefundPrice(), 0);
            //扣除会员积分
            memberService.updateMemberPoint(point.longValue(), false, afterSale.getMemberId(), "会员退款，扣除积分" + point + "分");

        }
    }

    /**
     * 获取积分设置
     *
     * @return 积分设置
     */
    private PointSetting getPointSetting() {
        Setting setting = settingService.get(SettingEnum.POINT_SETTING.name());
        return new Gson().fromJson(setting.getSettingValue(), PointSetting.class);
    }
}
