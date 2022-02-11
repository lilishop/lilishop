package cn.lili.modules.permission.entity.vo;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.permission.entity.dos.AdminUser;
import cn.lili.modules.permission.entity.dos.Menu;
import cn.lili.modules.permission.entity.dos.Role;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * 管理员VO
 *
 * @author Chopper
 * @since 2020-11-22 09:17
 */
@Data
public class AdminUserVO extends AdminUser {

    private static final long serialVersionUID = -2378384199695839312L;
    
    @ApiModelProperty(value = "所属部门名称")
    private String departmentTitle;

    @ApiModelProperty(value = "用户拥有角色")
    private List<Role> roles;

    @ApiModelProperty(value = "用户拥有的权限")
    private List<Menu> menus;


    public AdminUserVO(AdminUser user) {
        BeanUtil.copyProperties(user, this);
    }

}
