package cn.lili.modules.permission.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;


/**
 * 管理员类
 *
 * @author Chopper
 * @since 2020/11/19 11:42
 */
@Data
@TableName("li_admin_user")
@ApiModel(value = "管理员")
public class AdminUser extends BaseEntity {

    private static final long serialVersionUID = 2918352800205024873L;

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

    @ApiModelProperty(value = "用户头像")
    private String avatar = "https://i.loli.net/2020/11/19/LyN6JF7zZRskdIe.png";

    @ApiModelProperty(value = "是否是超级管理员 超级管理员/普通管理员")
    private Boolean isSuper = false;

    @ApiModelProperty(value = "状态 默认true正常 false禁用")
    private Boolean status = true;

    @ApiModelProperty(value = "描述/详情/备注")
    private String description;

    @ApiModelProperty(value = "所属部门id")
    private String departmentId;
    /**
     * 冗余字段
     */
    @ApiModelProperty(value = "角色id集合")
    private String roleIds;

}
