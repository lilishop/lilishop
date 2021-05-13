package cn.lili.modules.distribution.entity.dos;

import cn.lili.base.BaseEntity;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 分销员对象
 *
 * @author pikachu
 * @date 2020-03-14 23:04:56
 */
@Data
@Entity
@ApiModel(value = "分销员")
@TableName("li_distribution")
@Table(name = "li_distribution")
@NoArgsConstructor
public class Distribution extends BaseEntity {


    private static final long serialVersionUID = -4878132663540847325L;

    public Distribution(String memberId, String memberName, String name, String idNumber) {
        this.memberId = memberId;
        this.memberName = memberName;
        this.name = name;
        this.idNumber = idNumber;
        this.distributionStatus = DistributionStatusEnum.APPLY.name();
    }

    @ApiModelProperty(value = "会员id")
    private String memberId;

    @ApiModelProperty(value = "会员名称")
    private String memberName;

    @ApiModelProperty(value = "会员姓名")
    private String name;

    @ApiModelProperty(value = "身份证号")
    private String idNumber;

    @ApiModelProperty(value = "分销总额")
    private Double rebateTotal = 0D;

    @ApiModelProperty(value = "可提现金额")
    private Double canRebate = 0D;

    @ApiModelProperty(value = "冻结金额")
    private Double commissionFrozen = 0D;


    /**
     * @see DistributionStatusEnum
     */
    @ApiModelProperty(value = "分销员状态", required = true)
    private String distributionStatus;

}