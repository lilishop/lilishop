package cn.lili.modules.permission.entity.vo;

import cn.lili.modules.permission.entity.dos.Menu;
import lombok.Data;

/**
 * RoleMenuVO
 *
 * @author Chopper
 * @date 2020-11-24 11:45
 */
@Data
public class UserMenuVO extends Menu {

    private static final long serialVersionUID = -7478870595109016162L;

    /**
     * 是否是超级管理员
     */
    private Boolean isSupper;

}
