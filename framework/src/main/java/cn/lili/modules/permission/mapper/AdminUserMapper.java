package cn.lili.modules.permission.mapper;

import cn.lili.modules.permission.entity.dos.AdminUser;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;

import java.util.List;

/**
 * 用户数据处理层
 *
 * @author Chopper
 * @since 2020-11-22 09:17
 */
public interface AdminUserMapper extends BaseMapper<AdminUser> {

    /**
     * 通过用户名获取用户
     * @param username
     * @return
     */
    AdminUser findByUsername(String username);


    /**
     * 通过部门id获取
     * @param departmentId
     * @return
     */
    List<AdminUser> findByDepartmentId(String departmentId);

    /**
     * 通过用户名模糊搜索
     * @param username
     * @param status
     * @return
     */
    List<AdminUser> findByUsernameLikeAndStatus(String username, Integer status);
}
