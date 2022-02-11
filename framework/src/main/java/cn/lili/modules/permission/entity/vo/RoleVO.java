package cn.lili.modules.permission.entity.vo;

import cn.lili.modules.permission.entity.dos.Role;
import cn.lili.modules.permission.entity.dos.RoleMenu;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.List;

/**
 * RoleVO
 *
 * @author Chopper
 * @since 2020-11-22 17:42
 */
@Data
public class RoleVO extends Role {

    private static final long serialVersionUID = 8625345346785692513L;

    @ApiModelProperty(value = "拥有权限")
    private List<RoleMenu> roleMenus;
}
