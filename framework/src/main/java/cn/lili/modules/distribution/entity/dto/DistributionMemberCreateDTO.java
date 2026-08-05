package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

/**
 * 管理端新增分销员参数
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionMemberCreateDTO {

    @NotBlank(message = "昵称不能为空")
    @Length(max = 50, message = "昵称长度不能超过50")
    @Schema(description = "昵称")
    private String nickname;

    @NotBlank(message = "手机号不能为空")
    @Pattern(regexp = "^1[3-9]\\d{9}$", message = "手机号格式有误")
    @Schema(description = "手机号")
    private String mobile;

    @Schema(description = "邀请人手机号")
    private String inviterMobile;

    @NotBlank(message = "分销等级不能为空")
    @Schema(description = "分销等级ID")
    private String gradeId;
}
