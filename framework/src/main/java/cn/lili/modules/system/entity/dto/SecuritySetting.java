package cn.lili.modules.system.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 安全设置（数据脱敏、Token超时、演示站控制等）
 *
 * @author Chopper
 */
@Data
public class SecuritySetting implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "数据脱敏级别：0-不脱敏，1-管理端敏感信息脱敏，2-管理端与商家端同时脱敏")
    private Integer sensitiveLevel = 1;

    @Schema(description = "是否开启演示站点保护")
    private Boolean isDemoSite = false;

    @Schema(description = "买家端Token过期时间（单位：分钟）")
    private Long buyerTokenExpireTime = 259200L;

    @Schema(description = "管理端与商家端Token过期时间（单位：分钟）")
    private Long managerTokenExpireTime = 86400L;
}
