package cn.lili.modules.permission.service;

import cn.lili.modules.permission.entity.dos.DepartmentRole;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 部门角色业务层
 *
 * @author Chopper
 * @since 2020/11/22 12:08
 */
public interface DepartmentRoleService extends IService<DepartmentRole> {

    /**
     * 根据部门获取角色集合
     *
     * @param departmentId
     * @return
     */
    List<DepartmentRole> listByDepartmentId(String departmentId);


    /**
     * 更新部门角色关联
     *
     * @param departmentId
     * @param departmentRoles
     */
    void updateByDepartmentId(String departmentId, List<DepartmentRole> departmentRoles);

    /**
     * 根据部门id删除部门与角色关联
     *
     * @param ids id集合
     */
    void deleteByDepartment(List<String> ids);
}