package cn.lili.modules.permission.service;

import cn.lili.modules.permission.entity.dos.RoleMenu;
import cn.lili.modules.permission.entity.vo.UserMenuVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 角色菜单接口
 *
 * @author Chopper
 * @since 2020/11/22 11:43
 */
public interface RoleMenuService extends IService<RoleMenu> {

    /**
     * 通过角色获取菜单权限列表
     *
     * @param roleId
     * @return
     */
    List<RoleMenu> findByRoleId(String roleId);


    /**
     * 根据角色集合获取拥有的菜单具体权限
     *
     * @param userId
     * @return
     */
    List<UserMenuVO> findAllMenu(String userId);


    /**
     * 更新某角色拥有到菜单
     *
     * @param roleId
     * @param roleMenus
     */
    void updateRoleMenu(String roleId, List<RoleMenu> roleMenus);

    /**
     * 根据角色id 删除数据
     *
     * @param roleId
     */
    void deleteRoleMenu(String roleId);

    /**
     * 根据角色id 删除数据
     *
     * @param roleId
     */
    void deleteRoleMenu(List<String> roleId);

}