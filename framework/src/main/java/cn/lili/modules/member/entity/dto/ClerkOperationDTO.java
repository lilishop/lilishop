package cn.lili.modules.member.entity.dto;

import cn.lili.mybatis.BaseEntity;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

/**
 * 管理员入库dto
 *
 * @author Chopper
 * @since 2020/11/16 19:55
 */
@Data
@ApiModel(value = "店员操作dto")
public class ClerkOperationDTO extends BaseEntity {

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

    @ApiModelProperty(value = "头像")
    private String avatar;

    @ApiModelProperty(value = "描述/详情/备注")
    private String description;

    @ApiModelProperty(value = "所属部门id")
    private String departmentId;

    @ApiModelProperty(value = "是否为超级管理员")
    private Boolean isSuper;
}
