package cn.lili.modules.distribution.entity.dos;

import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 分销员对象
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Schema(description = "分销员")
@TableName("li_distribution")
@NoArgsConstructor
public class Distribution extends BaseEntity {


    private static final long serialVersionUID = -4878132663540847325L;

    public Distribution(String memberId, String memberName) {
        this.memberId = memberId;
        this.memberName = memberName;
        this.distributionOrderCount = 0;
        this.rebateTotal = 0D;
        this.canRebate = 0D;
        this.commissionFrozen = 0D;
        this.distributionOrderPrice = 0D;
        this.distributionStatus = DistributionStatusEnum.APPLY.name();
    }

    @Schema(description = "客户id")
    private String memberId;

    @Schema(description = "客户名称")
    private String memberName;

    @Schema(description = "分销总额")
    private Double rebateTotal = 0D;

    @Schema(description = "可提现金额")
    private Double canRebate = 0D;

    @Schema(description = "冻结金额")
    private Double commissionFrozen = 0D;

    @Schema(description = "分销订单数")
    private Integer distributionOrderCount;

    /**
     * @see DistributionStatusEnum
     */
    @Schema(description = "分销员状态", requiredMode = Schema.RequiredMode.REQUIRED)
    private String distributionStatus;

    @Schema(description = "分销订单金额")
    private Double distributionOrderPrice;

    @Schema(description = "分销等级值（1-5，对应等级配置中的等级值）")
    private String gradeId;

    @Schema(description = "上级分销员ID")
    private String parentDistributionId;

    @Schema(description = "邀请码")
    private String inviteCode;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "正式加入时间")
    private Date joinTime;

    @Schema(description = "累计有效销售额")
    private BigDecimal validSalesAmount;

    @Schema(description = "累计有效佣金")
    private BigDecimal validCommissionAmount;

    @Schema(description = "累计有效客户数")
    private Integer validCustomerCount;

    @Schema(description = "累计邀请数")
    private Integer inviteCount;

    @Schema(description = "提现冻结金额")
    private BigDecimal withdrawalFrozen;

    @Schema(description = "累计已提现金额")
    private BigDecimal withdrawnTotal;

}