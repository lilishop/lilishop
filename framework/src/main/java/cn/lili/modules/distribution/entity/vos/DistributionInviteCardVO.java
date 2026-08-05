package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 分销邀请卡（招募下级分销员）
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionInviteCardVO {

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "邀请码")
    private String inviteCode;

    @Schema(description = "展示名称")
    private String memberName;

    @Schema(description = "头像")
    private String memberAvatar;

    @Schema(description = "是否展示头像昵称")
    private Boolean memberInfoVisible;

    @Schema(description = "邀请卡背景图")
    private String backgroundImage;

    @Schema(description = "推荐语")
    private String slogan;

    @Schema(description = "文字颜色")
    private String textColor;

    @Schema(description = "小程序码跳转页面")
    private String sharePage;

    @Schema(description = "小程序码 scene 参数")
    private String shareScene;
}
