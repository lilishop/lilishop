package cn.lili.event.impl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.utils.BeanUtil;
import cn.lili.event.TradeEvent;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dos.Receipt;
import cn.lili.modules.order.order.entity.vo.OrderVO;
import cn.lili.modules.order.order.entity.vo.ReceiptVO;
import cn.lili.modules.order.order.service.ReceiptService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

/**
 * 订单创建发票相关处理
 *
 * @author Chopper
 * @since 2020-07-03 11:20
 **/
@Service
public class OrderCreateReceiptExecute implements TradeEvent {

    @Autowired
    private ReceiptService receiptService;

    @Override
    public void orderCreate(TradeDTO tradeDTO) {
        //根据交易sn查询订单信息
        List<OrderVO> orderList = tradeDTO.getOrderVO();
        //获取发票信息
        ReceiptVO receiptVO = tradeDTO.getReceiptVO();
        //如果需要获取发票则保存发票信息
        if (Boolean.TRUE.equals(tradeDTO.getNeedReceipt()) && !orderList.isEmpty()) {
            List<Receipt> receipts = new ArrayList<>();
            for (OrderVO orderVO : orderList) {
                Receipt receipt = new Receipt();
                BeanUtil.copyProperties(receiptVO, receipt);
                receipt.setMemberId(orderVO.getMemberId());
                receipt.setMemberName(orderVO.getMemberName());
                receipt.setStoreId(orderVO.getStoreId());
                receipt.setStoreName(orderVO.getStoreName());
                receipt.setOrderSn(orderVO.getSn());
                receipt.setReceiptDetail(JSONUtil.toJsonStr(orderVO.getOrderItems()));
                receipt.setReceiptPrice(orderVO.getFlowPrice());
                receipt.setReceiptStatus(0);
                receipts.add(receipt);
            }
            //保存发票
            receiptService.saveBatch(receipts);
        }
    }
}
