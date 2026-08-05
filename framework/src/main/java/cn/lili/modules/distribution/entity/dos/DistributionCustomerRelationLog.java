package cn.lili.modules.distribution.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销客户关系变更日志
 *
 * @author pikachu
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Schema(description = "分销客户关系变更日志")
@TableName("li_distribution_customer_relation_log")
@NoArgsConstructor
public class DistributionCustomerRelationLog extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "客户关系ID")
    private String relationId;

    @Schema(description = "客户会员ID")
    private String memberId;

    @Schema(description = "变更类型")
    private String changeType;

    @Schema(description = "变更前分销员ID")
    private String beforeDistributionId;

    @Schema(description = "变更后分销员ID")
    private String afterDistributionId;

    @Schema(description = "变更前关系状态")
    private String beforeRelationStatus;

    @Schema(description = "变更后关系状态")
    private String afterRelationStatus;

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

    @Schema(description = "操作者ID")
    private String operatorId;

    @Schema(description = "操作者类型")
    private String operatorType;
}
