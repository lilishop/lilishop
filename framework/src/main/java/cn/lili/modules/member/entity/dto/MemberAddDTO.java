package cn.lili.modules.member.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

/**
 * 添加会员DTO
 *
 * @author Bulbasaur
 * @since 2020/12/14 16:31
 */
@Data
public class MemberAddDTO {
    @NotEmpty(message = "会员用户名必填")
    @Size(max = 30,message = "会员用户名最长30位")
    @ApiModelProperty(value = "会员用户名")
    private String username;

    @ApiModelProperty(value = "会员密码")
    private String password;

    @NotEmpty(message = "手机号码不能为空")
    @ApiModelProperty(value = "手机号码", required = true)
    @Pattern(regexp = "^[1][3,4,5,6,7,8,9][0-9]{9}$", message = "手机号格式有误")
    private String mobile;
}
