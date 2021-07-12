package cn.lili.modules.permission.entity.dto;

import cn.lili.base.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Column;
import javax.validation.constraints.NotEmpty;

/**
 * 管理员入库dto
 *
 * @author Chopper
 * @date 2020/11/16 19:55
 */
@Data
@ApiModel(value = "管理员入库dto")
public class AdminUserDTO extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "用户名")
    @NotEmpty(message = "用户名不能为空")
    @Column(unique = true, nullable = false)
    private String username;

    @NotEmpty(message = "密码不能为空")
    @ApiModelProperty(value = "密码")
    private String password;

    @ApiModelProperty(value = "昵称")
    private String nickName;

    @ApiModelProperty(value = "手机")
    private String mobile;

    @ApiModelProperty(value = "邮件")
    private String email;

    @ApiModelProperty(value = "描述/详情/备注")
    private String description;

    @ApiModelProperty(value = "所属部门id")
    private String departmentId;

    @ApiModelProperty(value = "是否为超级管理员")
    private Boolean isSuper;
}
