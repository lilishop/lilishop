package cn.lili.modules.distribution.entity.vos;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销客户关系变更日志VO
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionCustomerRelationLogVO {

    @Schema(description = "日志ID")
    private String id;

    @Schema(description = "客户昵称")
    private String memberNickname;

    @Schema(description = "客户手机号")
    private String memberMobile;

    @Schema(description = "变更类型")
    private String changeType;

    @Schema(description = "变更类型描述")
    private String changeTypeLabel;

    @Schema(description = "变更内容说明")
    private String changeSummary;

    @Schema(description = "变更前分销员ID")
    private String beforeDistributionId;

    @Schema(description = "变更前分销员昵称")
    private String beforeDistributionNickname;

    @Schema(description = "变更前分销员手机号")
    private String beforeDistributionMobile;

    @Schema(description = "变更后分销员ID")
    private String afterDistributionId;

    @Schema(description = "变更后分销员昵称")
    private String afterDistributionNickname;

    @Schema(description = "变更后分销员手机号")
    private String afterDistributionMobile;

    @Schema(description = "变更前关系状态")
    private String beforeRelationStatus;

    @Schema(description = "变更前关系状态描述")
    private String beforeRelationStatusLabel;

    @Schema(description = "变更后关系状态")
    private String afterRelationStatus;

    @Schema(description = "变更后关系状态描述")
    private String afterRelationStatusLabel;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "变更前到期时间")
    private Date beforeExpireAt;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "变更后到期时间")
    private Date afterExpireAt;

    @Schema(description = "变更原因")
    private String reason;

    @Schema(description = "操作者类型")
    private String operatorType;

    @Schema(description = "操作者类型描述")
    private String operatorTypeLabel;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "变更时间")
    private Date createTime;
}
