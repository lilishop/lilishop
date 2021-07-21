package cn.lili.modules.permission.service;

import cn.lili.modules.permission.entity.dos.Department;
import cn.lili.modules.permission.entity.vo.DepartmentVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 部门业务层
 *
 * @author Chopper
 * @since 2020/11/17 3:43 下午
 */
public interface DepartmentService extends IService<Department> {

    /**
     * 获取部门树
     *
     * @param initWrapper
     * @return
     */
    List<DepartmentVO> tree(QueryWrapper<Department> initWrapper);

    /**
     * 删除部门
     *
     * @param ids
     */
    void deleteByIds(List<String> ids);
}