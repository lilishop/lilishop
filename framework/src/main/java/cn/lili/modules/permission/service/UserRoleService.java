package cn.lili.modules.permission.service;

import cn.lili.modules.permission.entity.dos.UserRole;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 管理员业务层
 *
 * @author Chopper
 * @since 2020/11/17 3:46 下午
 */
public interface UserRoleService extends IService<UserRole> {

    /**
     * 根据用户查看拥有的角色
     *
     * @param userId
     * @return
     */
    List<UserRole> listByUserId(String userId);

    /**
     * 根据用户查看拥有的角色
     *
     * @param userId
     * @return
     */
    List<String> listId(String userId);

    /**
     * 更新用户拥有的角色
     *
     * @param userId    角色
     * @param userRoles 角色权限
     */
    void updateUserRole(String userId, List<UserRole> userRoles);


}
