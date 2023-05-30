package cn.lili.modules.member.entity.vo;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.member.entity.dos.Clerk;
import cn.lili.modules.member.entity.dos.StoreMenu;
import cn.lili.modules.member.entity.dos.StoreRole;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * 管理员VO
 *
 * @author Chopper
 * @since 2020-11-22 09:17
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ClerkVO extends Clerk {

    private static final long serialVersionUID = -2378384199695839312L;

    @ApiModelProperty(value = "手机号")
    private String mobile;

    @ApiModelProperty(value = "所属部门名称")
    private String departmentTitle;

    @ApiModelProperty(value = "用户拥有角色")
    private List<StoreRole> roles;

    @ApiModelProperty(value = "用户拥有的权限")
    private List<StoreMenu> menus;


    public ClerkVO(Clerk clerk) {
        BeanUtil.copyProperties(clerk, this);
    }

}
