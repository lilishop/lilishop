package cn.lili.timetask.handler.impl.order;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.member.entity.dto.MemberEvaluationDTO;
import cn.lili.modules.member.entity.enums.EvaluationGradeEnum;
import cn.lili.modules.member.service.MemberEvaluationService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.OrderItemOperationDTO;
import cn.lili.modules.order.order.entity.enums.CommentStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
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

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author paulG
 * @since 2021/3/11
 **/
@Slf4j
@Component

public class OrderEveryDayTaskExecute implements EveryDayExecute {

    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;
    /**
     * 订单货物
     */
    @Autowired
    private OrderItemService orderItemService;
    /**
     * 设置
     */
    @Autowired
    private SettingService settingService;
    /**
     * 会员评价
     */
    @Autowired
    private MemberEvaluationService memberEvaluationService;

    @Autowired
    private DistributionOrderService distributionOrderService;

    /**
     * 执行每日任务
     */
    @Override
    public void execute() {

        Setting setting = settingService.get(SettingEnum.ORDER_SETTING.name());
        //订单设置
        OrderSetting orderSetting = JSONUtil.toBean(setting.getSettingValue(), OrderSetting.class);
        if (orderSetting == null) {
            throw new ServiceException(ResultCode.ORDER_SETTING_ERROR);
        }

        try {
            //自动确认收货
            completedOrder(orderSetting);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
        try {
            //自动好评
            memberEvaluation(orderSetting);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
        try {
            //关闭允许售后申请
            closeAfterSale(orderSetting);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
        try {
            //关闭允许投诉
            closeComplaint(orderSetting);
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
    }

    /**
     * 自动确认收获，订单完成
     *
     * @param orderSetting 订单设置
     */
    private void completedOrder(OrderSetting orderSetting) {


        //订单自动收货时间 = 当前时间 - 自动收货时间天数
        DateTime receiveTime = DateUtil.offsetDay(DateUtil.date(), -orderSetting.getAutoReceive());
        LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Order::getOrderStatus, OrderStatusEnum.DELIVERED.name());

        //订单发货时间 >= 订单自动收货时间
        queryWrapper.le(Order::getLogisticsTime, receiveTime);
        List<Order> list = orderService.list(queryWrapper);

        try {
            //判断是否有符合条件的订单，进行订单完成处理
            if (!list.isEmpty()) {
                List<String> receiveSnList = list.stream().map(Order::getSn).collect(Collectors.toList());
                for (String orderSn : receiveSnList) {
                    orderService.systemComplete(orderSn);
                }
            }
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
    }

    /**
     * 自动好评
     *
     * @param orderSetting 订单设置
     */
    private void memberEvaluation(OrderSetting orderSetting) {
        //订单自动收货时间 = 当前时间 - 自动收货时间天数
        DateTime receiveTime = DateUtil.offsetDay(DateUtil.date(), -orderSetting.getAutoEvaluation());

        //订单完成时间 <= 订单自动好评时间
        OrderItemOperationDTO orderItemOperationDTO = OrderItemOperationDTO.builder().receiveTime(receiveTime).commentStatus(CommentStatusEnum.UNFINISHED.name()).build();
        List<OrderItem> orderItems = orderItemService.waitOperationOrderItem(orderItemOperationDTO);

        //判断是否有符合条件的订单，进行自动评价处理
        if (!orderItems.isEmpty()) {
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

                try {
                    memberEvaluationService.addMemberEvaluation(memberEvaluationDTO, false);

                } catch (Exception e) {
                    log.error(e.getMessage(), e);
                }
            }
        }
    }


    /**
     * 关闭允许售后申请
     *
     * @param orderSetting 订单设置
     */
    private void closeAfterSale(OrderSetting orderSetting) {
        //为0则不限制
        if (orderSetting.getCloseAfterSale() == null || orderSetting.getCloseAfterSale() == 0) {
            return;
        }
        //订单关闭售后申请时间 = 当前时间 - 自动关闭售后申请天数
        DateTime receiveTime = DateUtil.offsetDay(DateUtil.date(), -orderSetting.getCloseAfterSale());
        //关闭售后订单=未售后订单+小于订单关闭售后申请时间
        OrderItemOperationDTO build = OrderItemOperationDTO.builder().receiveTime(receiveTime).afterSaleStatus(OrderItemAfterSaleStatusEnum.NOT_APPLIED.name()).build();
        List<OrderItem> orderItems = orderItemService.waitOperationOrderItem(build);

        //判断是否有符合条件的订单，关闭允许售后申请处理
        if (!orderItems.isEmpty()) {

            //获取订单货物ID
            List<String> orderItemIdList = orderItems.stream().map(OrderItem::getId).collect(Collectors.toList());

            //修改订单售后状态
            LambdaUpdateWrapper<OrderItem> lambdaUpdateWrapper = new LambdaUpdateWrapper<OrderItem>()
                    .set(OrderItem::getAfterSaleStatus, OrderItemAfterSaleStatusEnum.EXPIRED.name())
                    .in(OrderItem::getId, orderItemIdList);
            orderItemService.update(lambdaUpdateWrapper);
            orderItemService.expiredAfterSaleStatusExecuteByAfterSale(receiveTime);
            //修改订售后状态
            List<OrderItem> orderItemsList = orderItems.stream()
                    .map((orderItem) -> {
                        orderItem.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.EXPIRED.name());
                        return orderItem;
                    })
                    .collect(Collectors.toList());
            //修改对应分销订单状态
            distributionOrderService.updateDistributionOrderStatus(orderItemsList);
        }

    }

    /**
     * 关闭允许交易投诉
     *
     * @param orderSetting 订单设置
     */
    private void closeComplaint(OrderSetting orderSetting) {

        //为0则不限制
        if (orderSetting.getCloseComplaint() == null || orderSetting.getCloseComplaint() == 0) {
            return;
        }
        //订单关闭交易投诉申请时间 = 当前时间 - 自动关闭交易投诉申请天数
        DateTime receiveTime = DateUtil.offsetDay(DateUtil.date(), -orderSetting.getCloseComplaint());

        //关闭售后订单=未售后订单+小于订单关闭售后申请时间
        OrderItemOperationDTO build = OrderItemOperationDTO.builder().receiveTime(receiveTime).complainStatus(OrderComplaintStatusEnum.NO_APPLY.name()).build();
        List<OrderItem> orderItems = orderItemService.waitOperationOrderItem(build);

        //判断是否有符合条件的订单，关闭允许售后申请处理
        if (!orderItems.isEmpty()) {

            //获取订单货物ID
            List<String> orderItemIdList = orderItems.stream().map(OrderItem::getId).collect(Collectors.toList());

            //修改订单投诉状态
            LambdaUpdateWrapper<OrderItem> lambdaUpdateWrapper = new LambdaUpdateWrapper<OrderItem>()
                    .set(OrderItem::getComplainStatus, OrderItemAfterSaleStatusEnum.EXPIRED.name())
                    .in(OrderItem::getId, orderItemIdList);
            orderItemService.update(lambdaUpdateWrapper);
        }

    }

}
