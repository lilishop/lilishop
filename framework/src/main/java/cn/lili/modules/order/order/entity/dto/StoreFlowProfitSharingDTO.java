package cn.lili.modules.order.order.entity.dto;

import lombok.Data;

/**
 * 流水-实际分账DTO
 */
@Data
public class StoreFlowProfitSharingDTO {
    //平台获取金额
    private Double platformPrice;
    //店铺获取金额
    private Double storePrice;
    //分销员获取金额
    private Double distributionPrice;
    //合计金额  --剩余金额
    private Double price;
    //补差金额
    private Double subsidies;
}
