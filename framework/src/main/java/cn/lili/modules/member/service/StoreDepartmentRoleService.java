package cn.lili.modules.member.service;

import cn.lili.modules.member.entity.dos.StoreDepartmentRole;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 部门角色业务层
 *
 * @author Chopper
 * @since 2020/11/22 12:08
 */
public interface StoreDepartmentRoleService extends IService<StoreDepartmentRole> {

    /**
     * 根据部门获取角色集合
     *
     * @param storeDepartmentId 店铺部门id
     * @return
     */
    List<StoreDepartmentRole> listByDepartmentId(String storeDepartmentId);

    /**
     * 更新部门角色关联
     *
     * @param storeDepartmentId    店铺部门id
     * @param storeDepartmentRoles 店铺部门角色
     */
    void updateByDepartmentId(String storeDepartmentId, List<StoreDepartmentRole> storeDepartmentRoles);

    /**
     * 根据部门id删除部门与角色关联
     *
     * @param ids id集合
     */
    void deleteByDepartment(List<String> ids);
}