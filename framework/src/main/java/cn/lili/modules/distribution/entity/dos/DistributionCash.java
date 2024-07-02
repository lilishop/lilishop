package cn.lili.modules.distribution.entity.dos;

import cn.lili.modules.wallet.entity.enums.WithdrawStatusEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.validator.constraints.Length;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import javax.validation.constraints.NotBlank;
import java.util.Date;

/**
 * 分销佣金
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@Data
@TableName("li_distribution_cash")
@ApiModel(value = "分销佣金")
@NoArgsConstructor
@AllArgsConstructor
public class DistributionCash extends BaseEntity {


    private static final long serialVersionUID = -233580160480894288L;

    @ApiModelProperty(value = "分销佣金sn")
    private String sn;

    @ApiModelProperty(value = "分销员id")
    private String distributionId;

    @ApiModelProperty(value = "分销员名称")
    private String distributionName;

    @ApiModelProperty(value = "分销佣金")
    private Double price;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "支付时间")
    private Date payTime;

    @ApiModelProperty(value = "状态")
    private String distributionCashStatus;



    @ApiModelProperty(value = "会员姓名")
    private String name;
    @ApiModelProperty(value = "身份证号")
    private String idNumber;
    @ApiModelProperty(value = "结算银行开户行名称")
    private String settlementBankAccountName;
    @ApiModelProperty(value = "结算银行开户账号")
    private String settlementBankAccountNum;
    @ApiModelProperty(value = "结算银行开户支行名称")
    private String settlementBankBranchName;

    public DistributionCash(String sn,Double price, Distribution distribution) {
        this.sn = sn;
        this.distributionId = distribution.getId();
        this.price = price;
        this.distributionCashStatus = WithdrawStatusEnum.APPLY.name();
        this.distributionName = distribution.getMemberName();

        this.name = distribution.getName();
        this.idNumber = distribution.getIdNumber();
        this.settlementBankAccountName = distribution.getSettlementBankAccountName();
        this.settlementBankAccountNum = distribution.getSettlementBankAccountNum();
        this.settlementBankBranchName = distribution.getSettlementBankBranchName();
    }
}