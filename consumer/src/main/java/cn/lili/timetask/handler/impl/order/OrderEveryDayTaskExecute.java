package cn.lili.timetask.handler.impl.order;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.member.entity.dto.MemberEvaluationDTO;
import cn.lili.modules.member.entity.enums.EvaluationGradeEnum;
import cn.lili.modules.member.service.MemberEvaluationService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.OrderSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.timetask.handler.EveryDayExecute;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * @author paulG
 * @since 2021/3/11
 **/
@Slf4j
@Component

public class OrderEveryDayTaskExecute implements EveryDayExecute {

    //订单
    @Autowired
    private OrderService orderService;
    //订单货物
    @Autowired
    private OrderItemService orderItemService;
    //设置
    @Autowired
    private SettingService settingService;
    //会员评价
    @Autowired
    private MemberEvaluationService memberEvaluationService;

    /**
     * 执行每日任务
     */
    @Override
    public void execute() {
        Setting setting = settingService.get(SettingEnum.ORDER_SETTING.name());
        //自动确认收货
        OrderSetting orderSetting = JSONUtil.toBean(setting.getSettingValue(), OrderSetting.class);
        if (orderSetting == null) {
            throw new ServiceException(ResultCode.ORDER_SETTING_ERROR);
        }

        //自动确认收货
        completedOrder(orderSetting);
        //自动好评
        memberEvaluation(orderSetting);
    }

    /**
     * 自动确认收获，订单完成
     *
     * @param orderSetting 订单设置
     */
    private void completedOrder(OrderSetting orderSetting) {
        //订单自动收货时间 = 当前时间 - 自动收货时间天数
        DateTime receiveTime = DateUtil.offsetDay(DateUtil.date(), -orderSetting.getAutoEvaluation());
        LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Order::getOrderStatus, OrderStatusEnum.DELIVERED.name());
        //订单发货时间 >= 订单自动收货时间
        queryWrapper.ge(Order::getLogisticsTime, receiveTime);
        List<Order> list = orderService.list(queryWrapper);
        List<String> receiveSnList = list.stream().map(Order::getSn).collect(Collectors.toList());
        if (!receiveSnList.isEmpty()) {
            LambdaUpdateWrapper<Order> updateWrapper = new LambdaUpdateWrapper<>();
            updateWrapper.in(Order::getSn, receiveSnList);
            updateWrapper.set(Order::getOrderStatus, OrderStatusEnum.COMPLETED.name()).set(Order::getCompleteTime, new Date());
            boolean update = orderService.update(updateWrapper);
            if (Boolean.FALSE.equals(update)) {
                log.error("自动收货订单失败！订单编号为[{}]", receiveSnList);
            }
        }
    }

    /**
     * 自动好评
     *
     * @param orderSetting 订单设置
     */
    private void memberEvaluation(OrderSetting orderSetting) {
        //订单自动收货时间 = 当前时间 - 自动收货时间天数
        DateTime receiveTime = DateUtil.offsetDay(DateUtil.date(), -orderSetting.getAutoReceive());
        //订单完成时间 <= 订单自动好评时间
        List<OrderItem> orderItems = orderItemService.waitEvaluate(receiveTime);

        for (OrderItem orderItem : orderItems) {

            MemberEvaluationDTO memberEvaluationDTO = new MemberEvaluationDTO();
            memberEvaluationDTO.setOrderItemSn(orderItem.getSn());
            memberEvaluationDTO.setContent("系统默认好评");
            memberEvaluationDTO.setGoodsId(orderItem.getGoodsId());
            memberEvaluationDTO.setSkuId(orderItem.getSkuId());
            memberEvaluationDTO.setGrade(EvaluationGradeEnum.GOOD.name());
            memberEvaluationDTO.setDeliveryScore(5);
            memberEvaluationDTO.setDescriptionScore(5);
            memberEvaluationDTO.setServiceScore(5);

            memberEvaluationService.addMemberEvaluation(memberEvaluationDTO);

        }
    }


}
