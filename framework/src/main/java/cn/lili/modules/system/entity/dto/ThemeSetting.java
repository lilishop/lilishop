package cn.lili.modules.system.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 主题色配置
 */
@Data
public class ThemeSetting implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "主题色")
    private String themeColor;

    @Schema(description = "高亮主题色")
    private String lightColor;

    @Schema(description = "辅助高亮色")
    private String aiderLightColor;
}
