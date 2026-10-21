package cn.lili.modules.system.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 验证码设置
 *
 * @author Chopper
 */
@Data
public class VerificationSetting implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "有效时间（秒）")
    private Long effectiveTime = 300L;

    @Schema(description = "水印文字")
    private String watermark = "LILI-SHOP";

    @Schema(description = "干扰块数量")
    private Integer interfereNum = 0;

    @Schema(description = "容错像素")
    private Integer faultTolerant = 3;

    @Schema(description = "登录免验证码开关")
    private Boolean loginBypassEnabled = false;

    public Integer getInterfereNum() {
        if (interfereNum != null && interfereNum > 2) {
            return 2;
        }
        return interfereNum == null ? 0 : interfereNum;
    }
}
