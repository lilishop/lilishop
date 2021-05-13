package cn.lili.modules.statistics.model.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 消息提示
 *
 * @author Bulbasaur
 * @date 2020/12/9 14:25
 */
@Data
public class IndexNoticeVO {

    @ApiModelProperty(value = "待处理商品审核")
    private Integer goods;

    @ApiModelProperty(value = "待处理店铺入驻审核")
    private Integer store;

    @ApiModelProperty(value = "待处理售后申请")
    private Integer refund;

    @ApiModelProperty(value = "待处理投诉审核")
    private Integer complain;

    @ApiModelProperty(value = "待处理分销员提现申请")
    private Integer distributionCash;

    @ApiModelProperty(value = "待处理商家结算")
    private Integer waitPayBill;

}
