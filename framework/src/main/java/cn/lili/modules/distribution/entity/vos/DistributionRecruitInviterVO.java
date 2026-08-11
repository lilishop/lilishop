package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 招募页邀请人展示信息（无需登录）
 */
@Data
public class DistributionRecruitInviterVO {

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "邀请人昵称")
    private String memberName;

    @Schema(description = "邀请人头像")
    private String memberAvatar;
}
