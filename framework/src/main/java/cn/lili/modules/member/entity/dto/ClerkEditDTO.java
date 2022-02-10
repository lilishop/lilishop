package cn.lili.modules.member.entity.dto;

import cn.lili.common.security.sensitive.Sensitive;
import cn.lili.common.security.sensitive.enums.SensitiveStrategy;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import java.util.List;

/**
 * 店员dto
 *
 * @author wget
 * @title: Clerk
 * @projectName lilishop
 * @date 2021/12/28 7:39 下午
 */
@Data
@NoArgsConstructor
public class ClerkEditDTO {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "店员id", hidden = true)
    private String id;

    @ApiModelProperty(value = "会员密码")
    private String password;

    @ApiModelProperty(value = "状态")
    private Boolean status;

    @ApiModelProperty(value = "所属部门id")
    private String departmentId;

    @ApiModelProperty(value = "是否是超级管理员 超级管理员/普通管理员")
    private Boolean isSuper = false;

    @ApiModelProperty(value = "角色")
    private List<String> roles;


}
