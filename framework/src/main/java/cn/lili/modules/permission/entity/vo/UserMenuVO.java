package cn.lili.modules.permission.entity.vo;

import cn.lili.modules.permission.entity.dos.Menu;
import lombok.Data;

/**
 * RoleMenuVO
 *
 * @author Chopper
 * @since 2020-11-24 11:45
 */
@Data
public class UserMenuVO extends Menu {

    private static final long serialVersionUID = -7478870595109016162L;

    /**
     * 是否是超级管理员
     */
    private Boolean isSuper;

    public Boolean getSuper() {
        if (this.isSuper == null) {
            return false;
        }
        return isSuper;
    }
}
