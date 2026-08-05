package cn.lili.modules.distribution.entity.vos;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 分销员列表VO
 *
 * @author pikachu
 */
@Data
public class DistributionMemberVO {

    @Schema(description = "分销员ID")
    private String id;

    @Schema(description = "会员ID")
    private String memberId;

    @Schema(description = "手机号")
    private String mobile;

    @Schema(description = "昵称")
    private String nickname;

    @Schema(description = "分销等级值（1-5）")
    private String gradeId;

    @Schema(description = "等级名称")
    private String gradeName;

    @Schema(description = "邀请码")
    private String inviteCode;

    @Schema(description = "邀请方分销员ID")
    private String inviterDistributionId;

    @Schema(description = "邀请方昵称")
    private String inviterNickname;

    @Schema(description = "邀请方手机号")
    private String inviterMobile;

    @Schema(description = "累计有效销售额")
    private BigDecimal validSalesAmount;

    @Schema(description = "累计有效佣金")
    private BigDecimal validCommissionAmount;

    @Schema(description = "累计有效客户数")
    private Integer validCustomerCount;

    @Schema(description = "累计邀请数")
    private Integer inviteCount;

    @Schema(description = "可提现金额")
    private BigDecimal canRebate;

    @Schema(description = "佣金冻结金额")
    private BigDecimal commissionFrozen;

    @Schema(description = "提现冻结金额")
    private BigDecimal withdrawalFrozen;

    @Schema(description = "累计已提现金额")
    private BigDecimal withdrawnTotal;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionStatusEnum
     */
    @Schema(description = "分销员状态")
    private String distributionStatus;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "正式加入时间")
    private Date joinTime;
}
