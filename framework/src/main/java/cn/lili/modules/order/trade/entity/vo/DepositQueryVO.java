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
@ApiModel(value = "预存款变动记录查询条件")
@AllArgsConstructor
@NoArgsConstructor
public class DepositQueryVO implements Serializable {


    private static final long serialVersionUID = -6413611244037073693L;

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

    @ApiModelProperty(value = "起始日期")
    private String startDate;

    @ApiModelProperty(value = "结束日期")
    private String endDate;


}