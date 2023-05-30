package cn.lili.modules.order.order.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.order.order.aop.OrderLogPoint;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.mapper.TradeMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderPriceService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.payment.kit.plugin.bank.BankTransferPlugin;
import cn.lili.modules.system.aspect.annotation.SystemLogPoint;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.List;

/**
 * 订单价格业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 7:36 下午
 */
@Slf4j
@Service
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

    @Override
    @SystemLogPoint(description = "修改订单价格", customerLog = "'订单编号:'+#orderSn +'，价格修改为：'+#orderPrice")
    @OrderLogPoint(description = "'订单['+#orderSn+']修改价格，修改后价格为['+#orderPrice+']'", orderSn = "#orderSn")
    public Order updatePrice(String orderSn, Double orderPrice) {

        //修改订单金额
        Order order = updateOrderPrice(orderSn, orderPrice);

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

        //修改订单价格
        order.setUpdatePrice(CurrencyUtil.sub(orderPrice, orderPriceDetailDTO.getOriginalPrice()));

        //订单修改金额=使用订单原始金额-修改后金额
        orderPriceDetailDTO.setUpdatePrice(CurrencyUtil.sub(orderPrice, orderPriceDetailDTO.getOriginalPrice()));
        order.setFlowPrice(orderPriceDetailDTO.getFlowPrice());
        //修改订单
        order.setPriceDetailDTO(orderPriceDetailDTO);
        orderService.updateById(order);

        //修改子订单
        updateOrderItemPrice(order);

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

        //获取总数，入欧最后一个则将其他orderitem的修改金额累加，然后进行扣减
        Integer index = orderItems.size();
        Double countUpdatePrice = 0D;
        for (OrderItem orderItem : orderItems) {

            //获取订单货物价格信息
            PriceDetailDTO priceDetailDTO = orderItem.getPriceDetailDTO();

            index--;
            //如果是最后一个
            if (index == 0) {
                //记录修改金额
                priceDetailDTO.setUpdatePrice(CurrencyUtil.sub(order.getUpdatePrice(), countUpdatePrice));
                //修改订单货物金额
                orderItem.setFlowPrice(priceDetailDTO.getFlowPrice());
                orderItem.setUnitPrice(CurrencyUtil.div(priceDetailDTO.getFlowPrice(), orderItem.getNum()));
                orderItem.setPriceDetail(JSONUtil.toJsonStr(priceDetailDTO));

            } else {

                //SKU占总订单 金额的百分比
                Double priceFluctuationRatio = CurrencyUtil.div(priceDetailDTO.getOriginalPrice(), order.getPriceDetailDTO().getOriginalPrice(), 4);

                //记录修改金额
                priceDetailDTO.setUpdatePrice(CurrencyUtil.mul(order.getUpdatePrice(), priceFluctuationRatio));

                //修改订单货物金额
                orderItem.setFlowPrice(priceDetailDTO.getFlowPrice());
                orderItem.setUnitPrice(CurrencyUtil.div(priceDetailDTO.getFlowPrice(), orderItem.getNum()));
                orderItem.setPriceDetail(JSONUtil.toJsonStr(priceDetailDTO));
                countUpdatePrice = CurrencyUtil.add(countUpdatePrice, priceDetailDTO.getUpdatePrice());
            }
        }
        orderItemService.updateBatchById(orderItems);

    }

}
