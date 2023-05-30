package cn.lili.modules.order.trade.entity.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 预存款充值记录查询条件
 *
 * @author pikachu
 * @since 2020-02-25 14:10:16
 */
@Data
@ApiModel(value = "预存款充值记录查询条件")
@AllArgsConstructor
@NoArgsConstructor
public class RechargeQueryVO implements Serializable {


    private static final long serialVersionUID = 318396158590640917L;

    /**
     * 充值订单编号
     */
    @ApiModelProperty(value = "充值订单编号")
    private String rechargeSn;

    /**
     * 会员ID
     */
    @ApiModelProperty(value = "会员Id")
    private String memberId;
    /**
     * 会员名称
     */
    @ApiModelProperty(value = "会员名称")
    private String memberName;
    /**
     * 充值时间
     */
    @ApiModelProperty(value = "充值开始时间")
    private String startDate;

    /**
     * 充值时间
     */
    @ApiModelProperty(value = "充值结束时间")
    private String endDate;


}