package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 分销推广海报（邀请好友/锁客）
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionPromotionPosterVO {

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "展示名称")
    private String memberName;

    @Schema(description = "头像")
    private String memberAvatar;

    @Schema(description = "海报背景图")
    private String backgroundImage;

    @Schema(description = "海报副标题/标语")
    private String slogan;

    @Schema(description = "是否展示头像昵称")
    private Boolean memberInfoVisible;

    @Schema(description = "文字颜色")
    private String textColor;

    @Schema(description = "小程序码跳转页面")
    private String sharePage;

    @Schema(description = "小程序码 scene 参数")
    private String shareScene;
}
