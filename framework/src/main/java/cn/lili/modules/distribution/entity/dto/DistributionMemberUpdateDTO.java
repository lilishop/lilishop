package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

/**
 * 管理端分销员编辑参数
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionMemberUpdateDTO {

    @NotBlank(message = "昵称不能为空")
    @Length(max = 50, message = "昵称长度不能超过50")
    @Schema(description = "昵称")
    private String nickname;

    @Schema(description = "分销等级ID")
    private String gradeId;

    @Schema(description = "邀请人手机号，传空字符串表示清除邀请人")
    private String inviterMobile;
}
