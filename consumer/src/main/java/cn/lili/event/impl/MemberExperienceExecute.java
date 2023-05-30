package cn.lili.event.impl;


import cn.lili.common.utils.CurrencyUtil;
import cn.lili.event.GoodsCommentCompleteEvent;
import cn.lili.event.MemberRegisterEvent;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.ExperienceSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.google.gson.Gson;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * 会员经验值
 *
 * @author Bulbasaur
 * @since 2021/5/16 11:16 下午
 */
//@Service
public class MemberExperienceExecute implements MemberRegisterEvent, GoodsCommentCompleteEvent, OrderStatusChangeEvent {

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
     * 会员注册赠送经验值
     *
     * @param member 会员
     */
    @Override
    public void memberRegister(Member member) {
        //获取经验值设置
        ExperienceSetting experienceSetting = getExperienceSetting();
        //赠送会员经验值
        memberService.updateMemberPoint(Long.valueOf(experienceSetting.getRegister().longValue()), PointTypeEnum.INCREASE.name(), member.getId(), "会员注册，赠送经验值" + experienceSetting.getRegister());
    }

    /**
     * 商品评价赠送经验值
     *
     * @param memberEvaluation 会员评价
     */
    @Override
    public void goodsComment(MemberEvaluation memberEvaluation) {
        //获取经验值设置
        ExperienceSetting experienceSetting = getExperienceSetting();
        //赠送会员经验值
        memberService.updateMemberPoint(Long.valueOf(experienceSetting.getComment().longValue()), PointTypeEnum.INCREASE.name(), memberEvaluation.getMemberId(), "会员评价，赠送经验值" + experienceSetting.getComment());
    }

    /**
     * 完成订单赠送经验值
     *
     * @param orderMessage 订单消息
     */
    @Override
    public void orderChange(OrderMessage orderMessage) {
        if (orderMessage.getNewStatus().equals(OrderStatusEnum.COMPLETED)) {
            //获取经验值设置
            ExperienceSetting experienceSetting = getExperienceSetting();
            //获取订单信息
            Order order = orderService.getBySn(orderMessage.getOrderSn());
            //计算赠送经验值数量
            Double point = CurrencyUtil.mul(experienceSetting.getMoney(), order.getFlowPrice(), 0);
            //赠送会员经验值
            memberService.updateMemberPoint(point.longValue(), PointTypeEnum.INCREASE.name(), order.getMemberId(), "会员下单，赠送经验值" + point + "分");
        }
    }

    /**
     * 获取经验值设置
     *
     * @return 经验值设置
     */
    private ExperienceSetting getExperienceSetting() {
        Setting setting = settingService.get(SettingEnum.EXPERIENCE_SETTING.name());
        return new Gson().fromJson(setting.getSettingValue(), ExperienceSetting.class);
    }
}
