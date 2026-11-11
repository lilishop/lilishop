package cn.lili.event.impl;


import cn.lili.event.GoodsCommentCompleteEvent;
import cn.lili.event.MemberRegisterEvent;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.member.entity.enums.PointSourceEnum;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.PointSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import com.google.gson.Gson;
import lombok.extern.slf4j.Slf4j;
import java.util.concurrent.TimeUnit;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 客户积分
 *
 * @author Bulbasaur
 * @since 2020-07-03 11:20
 */
@Slf4j
@Service
public class MemberPointExecute implements MemberRegisterEvent, GoodsCommentCompleteEvent, OrderStatusChangeEvent {


    @Autowired
    private SettingService settingService;
    @Autowired
    private MemberService memberService;
    @Autowired
    private OrderService orderService;
    @Autowired(required = false)
    private Cache cache;

    /**
     * 客户注册赠送积分
     *
     * @param member 客户
     */
    @Override
    public void memberRegister(Member member) {
        //获取积分设置
        PointSetting pointSetting = getPointSetting();
        //赠送客户积分
        memberService.updateMemberPoint(pointSetting.getRegister().longValue(), PointTypeEnum.INCREASE.name(), member.getId(), "客户注册，赠送积分" + pointSetting.getRegister() + "分", PointSourceEnum.REGISTER.name());
    }

    /**
     * 客户完成评价后按积分配置赠送评价积分，提货卡订单与普通订单一致，不按支付方式排除。
     *
     * @param memberEvaluation 客户评价
     */
    @Override
    public void goodsComment(MemberEvaluation memberEvaluation) {
        //获取积分设置
        PointSetting pointSetting = getPointSetting();
        //赠送客户积分
        memberService.updateMemberPoint(pointSetting.getComment().longValue(), PointTypeEnum.INCREASE.name(), memberEvaluation.getMemberId(), "客户评价，赠送积分" + pointSetting.getComment() + "分", PointSourceEnum.COMMENT.name());
    }

    /**
     * 处理订单取消时返还支付积分；购物赠送积分统一由关闭售后任务发放
     *
     * @param orderMessage 订单消息
     */
    @Override
    public void orderChange(OrderMessage orderMessage) {

        switch (orderMessage.getNewStatus()) {
            case CANCELLED: {
                Order order = orderService.getBySn(orderMessage.getOrderSn());
                if (order == null || order.getPriceDetailDTO() == null) {
                    return;
                }
                Long point = order.getPriceDetailDTO().getPayPoint();
                if (point == null || point <= 0 || CharSequenceUtil.isBlank(order.getMemberId())) {
                    return;
                }
                String cacheKey = "order:point:refund:" + order.getSn();
                if (cache != null && Boolean.TRUE.equals(cache.hasKey(cacheKey))) {
                    log.info("订单[{}]取消积分已返还，跳过重复处理", order.getSn());
                    return;
                }
                String content = "订单取消，积分返还：" + point + "分";
                // 归还客户积分（下单时已扣减，取消时无论是否付款均应返还）
                Boolean success = memberService.updateMemberPoint(point, PointTypeEnum.INCREASE.name(), order.getMemberId(), content, PointSourceEnum.ORDER.name());
                if (Boolean.TRUE.equals(success) && cache != null) {
                    cache.put(cacheKey, 1, 7L, TimeUnit.DAYS);
                }
                break;
            }
            default:
                break;
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
