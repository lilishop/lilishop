package cn.lili.modules.wallet.entity.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 余额提现记录查询条件
 *
 * @author pikachu
 * @since 2020-02-25 14:10:16
 */
@Data
@ApiModel(value = "余额提现记录查询条件")
@AllArgsConstructor
@NoArgsConstructor
public class MemberWithdrawApplyQueryVO implements Serializable {


    private static final long serialVersionUID = 4735408873104054674L;

    /**
     * 充值订单编号
     */
    @ApiModelProperty(value = "充值订单编号")
    private String sn;

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
     * 提现申请状态
     */
    @ApiModelProperty(value = "提现申请状态")
    private String applyStatus;
    /**
     * 提现申请时间
     */
    @ApiModelProperty(value = "提现申请时间起始日期")
    private String startDate;
    /**
     * 提现申请时间
     */
    @ApiModelProperty(value = "提现申请时间结束日期")
    private String endDate;


}