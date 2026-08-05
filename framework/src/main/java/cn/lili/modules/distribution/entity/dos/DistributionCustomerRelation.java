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
 * 分销客户当前关系
 *
 * @author pikachu
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Schema(description = "分销客户当前关系")
@TableName("li_distribution_customer_relation")
@NoArgsConstructor
public class DistributionCustomerRelation extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "客户会员ID")
    private String memberId;

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "绑定来源")
    private String bindSource;

    @Schema(description = "邀请码")
    private String inviteCode;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionBindModeEnum
     */
    @Schema(description = "锁客模式快照")
    private String bindMode;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionPermanentBindModeEnum
     */
    @Schema(description = "永久锁客方式快照")
    private String permanentBindMode;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionRelationStatusEnum
     */
    @Schema(description = "关系状态")
    private String relationStatus;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "绑定时间")
    private Date bindTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "最后扫码时间")
    private Date lastScanTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "到期时间")
    private Date expireAt;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "永久生效时间")
    private Date permanentAt;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "待转永久截止时间")
    private Date pendingPermanentDeadline;

    @Schema(description = "触发转永久的订单号")
    private String triggerOrderSn;

    @Schema(description = "是否星标客户")
    private Boolean starred;
}
