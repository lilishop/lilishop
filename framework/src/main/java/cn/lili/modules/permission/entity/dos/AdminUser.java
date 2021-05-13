package cn.lili.modules.permission.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 管理员类
 *
 * @author Chopper
 * @date 2020/11/19 11:42
 */
@Data
@Entity
@Table(name = "li_admin_user")
@TableName("li_admin_user")
@ApiModel(value = "管理员")
public class AdminUser extends BaseEntity {

    private static final long serialVersionUID = 2918352800205024873L;

    @ApiModelProperty(value = "用户名")
    @Column(unique = true, nullable = false, columnDefinition = "varchar(200)")
    private String username;

    @ApiModelProperty(value = "密码")
    private String password;

    @ApiModelProperty(value = "昵称")
    private String nickName;

    @ApiModelProperty(value = "手机")
    private String mobile;

    @ApiModelProperty(value = "邮件")
    private String email;

    @ApiModelProperty(value = "用户头像")
    @Column(length = 1000)
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
