package cn.lili.timetask.handler.impl.order;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.member.entity.dto.MemberEvaluationDTO;
import cn.lili.modules.member.entity.enums.EvaluationGradeEnum;
import cn.lili.modules.member.service.MemberEvaluationService;
import cn.lili.modules.order.aftersale.service.AfterSaleService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.CommentStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderComplaintStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.mapper.OrderItemMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.OrderSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.timetask.handler.EveryDayExecute;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.Resource;
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
    @Resource
    private OrderItemMapper orderItemMapper;
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
    private AfterSaleService afterSaleService;

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

        //自动确认收货
        completedOrder(orderSetting);
        //自动好评
        memberEvaluation(orderSetting);
        //关闭允许售后申请
        closeAfterSale(orderSetting);
        //关闭允许投诉
        closeComplaint(orderSetting);
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

        //判断是否有符合条件的订单，进行订单完成处理
        if (!list.isEmpty()) {
            List<String> receiveSnList = list.stream().map(Order::getSn).collect(Collectors.toList());
            for (String orderSn : receiveSnList) {
                orderService.systemComplete(orderSn);
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
        DateTime receiveTime = DateUtil.offsetDay(DateUtil.date(), -orderSetting.getAutoEvaluation());

        //订单完成时间 <= 订单自动好评时间
        QueryWrapper queryWrapper = new QueryWrapper();
        queryWrapper.le("o.complete_time", receiveTime);
        queryWrapper.eq("oi.comment_status", CommentStatusEnum.UNFINISHED.name());
        List<OrderItem> orderItems = orderItemMapper.waitOperationOrderItem(queryWrapper);

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

                memberEvaluationService.addMemberEvaluation(memberEvaluationDTO, false);
            }
        }
    }


    /**
     * 关闭允许售后申请
     *
     * @param orderSetting 订单设置
     */
    private void closeAfterSale(OrderSetting orderSetting) {

        //订单关闭售后申请时间 = 当前时间 - 自动关闭售后申请天数
        DateTime receiveTime = DateUtil.offsetDay(DateUtil.date(), -orderSetting.getAutoEvaluation());

        //关闭售后订单=未售后订单+小于订单关闭售后申请时间
        QueryWrapper queryWrapper = new QueryWrapper();
        queryWrapper.le("o.complete_time", receiveTime);
        queryWrapper.eq("oi.after_sale_status", OrderItemAfterSaleStatusEnum.NOT_APPLIED.name());
        List<OrderItem> orderItems = orderItemMapper.waitOperationOrderItem(queryWrapper);

        //判断是否有符合条件的订单，关闭允许售后申请处理
        if (!orderItems.isEmpty()) {

            //获取订单货物ID
            List<String> orderItemIdList = orderItems.stream().map(OrderItem::getId).collect(Collectors.toList());

            //修改订单售后状态
            LambdaUpdateWrapper<OrderItem> lambdaUpdateWrapper = new LambdaUpdateWrapper<OrderItem>()
                    .set(OrderItem::getAfterSaleStatus, OrderItemAfterSaleStatusEnum.EXPIRED.name())
                    .in(OrderItem::getId, orderItemIdList);
            orderItemService.update(lambdaUpdateWrapper);
        }

    }

    /**
     * 关闭允许交易投诉
     *
     * @param orderSetting 订单设置
     */
    private void closeComplaint(OrderSetting orderSetting) {

        //订单关闭交易投诉申请时间 = 当前时间 - 自动关闭交易投诉申请天数
        DateTime receiveTime = DateUtil.offsetDay(DateUtil.date(), -orderSetting.getCloseComplaint());

        //关闭售后订单=未售后订单+小于订单关闭售后申请时间
        QueryWrapper queryWrapper = new QueryWrapper();
        queryWrapper.le("o.complete_time", receiveTime);
        queryWrapper.eq("oi.complain_status", OrderComplaintStatusEnum.NO_APPLY.name());
        List<OrderItem> orderItems = orderItemMapper.waitOperationOrderItem(queryWrapper);

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
