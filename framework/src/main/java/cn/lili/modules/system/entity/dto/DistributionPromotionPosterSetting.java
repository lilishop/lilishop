package cn.lili.modules.system.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 分销平台海报设置
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionPromotionPosterSetting implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "是否展示头像昵称")
    private Boolean memberInfoVisible = Boolean.TRUE;

    @Schema(description = "海报背景图")
    private String backgroundImage;

    @Schema(description = "推荐语")
    private String slogan = "发现好物，邀请你一起逛逛";

    @Schema(description = "文字颜色（十六进制）")
    private String textColor = "#1f2a44";
}
