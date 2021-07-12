package cn.lili.modules.order.order.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.aop.syslog.annotation.SystemLogPoint;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.OperationalJudgment;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.order.order.aop.OrderLogPoint;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.mapper.TradeMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderPriceService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.payment.kit.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.plugin.bank.BankTransferPlugin;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.util.List;

/**
 * 订单价格业务层实现
 *
 * @author Chopper
 * @date 2020/11/17 7:36 下午
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class OrderPriceServiceImpl implements OrderPriceService {

    /**
     * 线下收款
     */
    @Autowired
    private BankTransferPlugin bankTransferPlugin;
    /**
     * 订单货物
     */
    @Autowired
    private OrderItemService orderItemService;
    /**
     * 交易数据层
     */
    @Resource
    private TradeMapper tradeMapper;
    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;
    /**
     * 商品分类
     */
    @Autowired
    private CategoryService categoryService;

    @Override
    @SystemLogPoint(description = "修改订单价格", customerLog = "'订单编号:'+#orderSn +'，价格修改为：'+#orderPrice")
    @OrderLogPoint(description = "'订单['+#orderSn+']修改价格，修改后价格为['+#orderPrice+']'", orderSn = "#orderSn")
    public Order updatePrice(String orderSn, Double orderPrice) {

        //修改订单金额
        Order order = updateOrderPrice(orderSn, orderPrice);

        //修改订单货物金额
        updateOrderItemPrice(order);

        //修改交易金额
        tradeMapper.updateTradePrice(order.getTradeSn());
        return order;
    }

    @Override
    @OrderLogPoint(description = "'管理员操作订单['+#orderSn+']付款'", orderSn = "#orderSn")
    public void adminPayOrder(String orderSn) {
        Order order = OperationalJudgment.judgment(orderService.getBySn(orderSn));
        //如果订单已付款，则抛出异常
        if (order.getPayStatus().equals(PayStatusEnum.PAID.name())) {
            throw new ServiceException(ResultCode.PAY_DOUBLE_ERROR);
        }

        bankTransferPlugin.callBack(order);

        //发送订单状态变化消息
        OperationalJudgment.judgment(orderService.getBySn(orderSn));
        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setOrderSn(orderSn);
        orderMessage.setPaymentMethod(PaymentMethodEnum.BANK_TRANSFER.name());
        orderMessage.setNewStatus(OrderStatusEnum.PAID);
        orderService.sendUpdateStatusMessage(orderMessage);
    }


    /**
     * 修改订单价格
     * 1.判定订单是否支付
     * 2.记录订单原始价格信息
     * 3.计算修改的订单金额
     * 4.修改订单价格
     * 5.保存订单信息
     *
     * @param orderSn    订单编号
     * @param orderPrice 修改订单金额
     */
    private Order updateOrderPrice(String orderSn, Double orderPrice) {
        Order order = OperationalJudgment.judgment(orderService.getBySn(orderSn));
        //判定是否支付
        if (order.getPayStatus().equals(PayStatusEnum.PAID.name())) {
            throw new ServiceException(ResultCode.ORDER_UPDATE_PRICE_ERROR);
        }

        //获取订单价格信息
        PriceDetailDTO orderPriceDetailDTO = order.getPriceDetailDTO();

        //如果未修改过金额则记录订单原始价格
        if (orderPriceDetailDTO.getOriginalPrice() == null) {
            orderPriceDetailDTO.setOriginalPrice(order.getFlowPrice());
        }

        //修改订单价格
        order.setFlowPrice(orderPrice);
        //订单修改金额=使用订单原始金额-修改后金额
        orderPriceDetailDTO.setUpdatePrice(CurrencyUtil.sub(orderPriceDetailDTO.getOriginalPrice(), orderPrice));
        orderPriceDetailDTO.setFlowPrice(orderPrice);
        //修改订单
        order.setPriceDetail(JSONUtil.toJsonStr(orderPriceDetailDTO));
        orderService.updateById(order);
        return order;
    }

    /**
     * 修改订单货物金额
     * 1.计算订单货物金额在订单金额中的百分比
     * 2.订单货物金额=订单修改后金额*订单货物百分比
     * 3.订单货物修改价格=订单货物原始价格-订单货物修改后金额
     * 4.修改平台佣金
     * 5.订单实际金额=修改后订单金额-平台佣金-分销提佣
     *
     * @param order 订单
     */
    private void updateOrderItemPrice(Order order) {
        List<OrderItem> orderItems = orderItemService.getByOrderSn(order.getSn());
        for (OrderItem orderItem : orderItems) {

            //获取订单货物价格信息
            PriceDetailDTO priceDetailDTO = orderItem.getPriceDetailDTO();

            //如果未修改过金额则记录订单原始价格
            if (priceDetailDTO.getOriginalPrice() == null) {
                priceDetailDTO.setOriginalPrice(orderItem.getFlowPrice());
            }

            //获取订单货物占订单金额的百分比
            Double priceFluctuationRatio = CurrencyUtil.div(priceDetailDTO.getOriginalPrice(), order.getPriceDetailDTO().getOriginalPrice());

            //计算修改后的订单货物金额
            double flowPrice = CurrencyUtil.mul(order.getFlowPrice(), priceFluctuationRatio);

            //记录修改金额
            priceDetailDTO.setUpdatePrice(CurrencyUtil.sub(priceDetailDTO.getOriginalPrice(), flowPrice));
            priceDetailDTO.setFlowPrice(flowPrice);

            //计算平台佣金=交易金额*分类佣金比例/100
            Double platFormCommission = CurrencyUtil.div(CurrencyUtil.mul(flowPrice, categoryService.getById(orderItem.getCategoryId()).getCommissionRate()), 100);
            priceDetailDTO.setPlatFormCommission(platFormCommission);

            //最终结算金额 = 流水金额-平台佣金-分销提佣
            double billPrice = CurrencyUtil.sub(CurrencyUtil.sub(priceDetailDTO.getFlowPrice(), priceDetailDTO.getPlatFormCommission()),
                    priceDetailDTO.getDistributionCommission());
            priceDetailDTO.setBillPrice(billPrice);

            //修改订单货物金额
            orderItem.setFlowPrice(flowPrice);
            orderItem.setPriceDetail(JSONUtil.toJsonStr(priceDetailDTO));
            orderItemService.update(orderItem, new LambdaUpdateWrapper<OrderItem>().eq(OrderItem::getId, orderItem.getId()));
        }
    }

}
