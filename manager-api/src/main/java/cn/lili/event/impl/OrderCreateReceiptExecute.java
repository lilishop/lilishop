package cn.lili.event.impl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.utils.BeanUtil;
import cn.lili.event.TradeEvent;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dos.Receipt;
import cn.lili.modules.order.order.entity.enums.ReceiptInvoiceTypeEnum;
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
                receipt.setReceiptType(ReceiptInvoiceTypeEnum.toStoredLabel(receiptVO.getReceiptType()));
                receipt.setReceiptTitle(normalizeReceiptTitle(receiptVO));
                fillTitleNames(receipt, receiptVO);
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

    /**
     * 统一发票抬头：仅落库“个人/单位”，避免名称类文案混入抬头字段。
     */
    private String normalizeReceiptTitle(ReceiptVO receiptVO) {
        if (receiptVO == null) {
            return "个人";
        }
        String taxpayerId = receiptVO.getTaxpayerId() == null ? "" : receiptVO.getTaxpayerId().trim();
        if (!taxpayerId.isEmpty()) {
            return "单位";
        }
        String receiptTypeLabel = ReceiptInvoiceTypeEnum.toStoredLabel(receiptVO.getReceiptType());
        if (ReceiptInvoiceTypeEnum.VAT_SPECIAL.getLabel().equals(receiptTypeLabel)) {
            return "单位";
        }
        return "个人";
    }

    /**
     * 将抬头名称拆分存储到 personalName/companyName。
     * - 新协议：优先使用 receiptVO.personalName/companyName
     * - 旧协议：receiptVO.receiptTitle 仍是具体名称时，按抬头类型回填
     */
    private void fillTitleNames(Receipt receipt, ReceiptVO receiptVO) {
        if (receiptVO == null || receipt == null) {
            return;
        }
        String rawTitle = receiptVO.getReceiptTitle() == null ? "" : receiptVO.getReceiptTitle().trim();
        String personalName = receiptVO.getPersonalName() == null ? "" : receiptVO.getPersonalName().trim();
        String companyName = receiptVO.getCompanyName() == null ? "" : receiptVO.getCompanyName().trim();
        boolean isCompanyTitle = "单位".equals(receipt.getReceiptTitle());

        if (isCompanyTitle) {
            if (companyName.isEmpty() && !"个人".equals(rawTitle) && !"单位".equals(rawTitle)) {
                companyName = rawTitle;
            }
            receipt.setCompanyName(companyName);
            receipt.setPersonalName("");
        } else {
            if (personalName.isEmpty() && !"个人".equals(rawTitle) && !"单位".equals(rawTitle)) {
                personalName = rawTitle;
            }
            receipt.setPersonalName(personalName);
            receipt.setCompanyName("");
        }
    }
}
