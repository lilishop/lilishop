package cn.lili.modules.distribution.entity.vos;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 我邀请的下级分销员
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionInviteeVO {

    @Schema(description = "下级分销员ID")
    private String childDistributionId;

    @Schema(description = "展示名称")
    private String memberName;

    @Schema(description = "头像")
    private String memberAvatar;

    @Schema(description = "等级名称")
    private String gradeName;

    @Schema(description = "邀请奖励金额")
    private BigDecimal inviteRewardAmount;

    @Schema(description = "销售额")
    private BigDecimal salesAmount;

    @Schema(description = "绑客数")
    private Integer customerCount;

    @Schema(description = "订单数")
    private Integer orderCount;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "成为下级时间")
    private Date effectiveTime;
}
