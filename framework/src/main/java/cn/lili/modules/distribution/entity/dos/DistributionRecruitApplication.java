package cn.lili.modules.distribution.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销招募申请
 *
 * @author pikachu
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Schema(description = "分销招募申请")
@TableName("li_distribution_recruit_application")
@NoArgsConstructor
public class DistributionRecruitApplication extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "会员ID")
    private String memberId;

    @Schema(description = "分销招募设置ID")
    private String planId;

    @Schema(description = "分销招募设置版本号")
    @TableField("plan_version_no")
    private Integer planVersion;

    @Schema(description = "申请来源：MANUAL_ENTRY AUTO_ENTRY ADMIN")
    @TableField("apply_source")
    private String applySource;

    @Schema(description = "审核方式快照")
    @TableField("audit_mode_snapshot")
    private String auditMode;

    @Schema(description = "申请状态")
    private String applicationStatus;

    @Schema(description = "招募邀请令牌")
    private String recruitToken;

    @Schema(description = "招募邀请方分销员ID")
    private String recruitInviterId;

    @Schema(description = "原客户绑定分销员ID")
    private String boundDistributorId;

    @Schema(description = "申请时服务协议内容快照（富文本）")
    private String agreementContent;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "协议接受时间")
    private Date agreementAcceptTime;

    @Schema(description = "条件完成快照(JSON)")
    private String conditionSnapshot;

    @Schema(description = "动态填写内容快照(JSON)")
    @TableField("apply_form_snapshot")
    private String applyFormSnapshot;

    @Schema(description = "审核人ID")
    private String auditUserId;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "审核时间")
    private Date auditTime;

    @Schema(description = "审核备注")
    @TableField(exist = false)
    private String auditRemark;

    @Schema(description = "拒绝原因")
    private String rejectReason;

    @Schema(description = "关联分销员ID")
    private String distributionId;
}
