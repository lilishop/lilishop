package cn.lili.modules.distribution.entity.vos;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销客户关系VO
 *
 * @author pikachu
 */
@Data
public class DistributionCustomerRelationVO {

    @Schema(description = "关系ID")
    private String id;

    @Schema(description = "客户会员ID")
    private String memberId;

    @Schema(description = "客户手机号")
    private String memberMobile;

    @Schema(description = "客户昵称")
    private String memberNickname;

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "分销员手机号")
    private String distributionMobile;

    @Schema(description = "分销员昵称")
    private String distributionNickname;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionRelationStatusEnum
     */
    @Schema(description = "当前关系状态")
    private String relationStatus;

    @Schema(description = "当前关系是否有效")
    private Boolean relationEffective;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "失效时间")
    private Date invalidTime;

    @Schema(description = "失效原因")
    private String invalidReason;

    @Schema(description = "是否有截止日期")
    private Boolean hasDeadline;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "绑定时间")
    private Date bindTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "有效截止日期")
    private Date expireAt;

    @Schema(description = "绑定来源")
    private String bindSource;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionBindModeEnum
     */
    @Schema(description = "锁客模式快照")
    private String bindMode;
}
