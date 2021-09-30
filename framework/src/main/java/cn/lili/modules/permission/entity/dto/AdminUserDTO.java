package cn.lili.modules.permission.entity.dto;

import cn.lili.mybatis.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;

/**
 * 管理员入库dto
 *
 * @author Chopper
 * @since 2020/11/16 19:55
 */
@Data
@ApiModel(value = "管理员入库dto")
public class AdminUserDTO extends BaseEntity {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty(value = "用户名")
    @Length(max = 20,message = "用户名长度不能超过20个字符")
    private String username;

    @ApiModelProperty(value = "密码")
    private String password;

    @ApiModelProperty(value = "昵称")
    @Length(max = 10,message = "昵称长度不能超过10个字符")
    private String nickName;

    @ApiModelProperty(value = "手机")
    @Length(max = 11,message = "手机号长度不能超过11")
    private String mobile;

    @ApiModelProperty(value = "邮件")
    @Length(max = 100,message = "邮箱长度不能超过100")
    private String email;

    @ApiModelProperty(value = "头像")
    private String avatar;

    @ApiModelProperty(value = "描述/详情/备注")
    private String description;

    @ApiModelProperty(value = "所属部门id")
    private String departmentId;

    @ApiModelProperty(value = "是否为超级管理员")
    private Boolean isSuper;
}
