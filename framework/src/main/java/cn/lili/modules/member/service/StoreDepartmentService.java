package cn.lili.modules.member.service;

import cn.lili.modules.member.entity.dos.StoreDepartment;
import cn.lili.modules.member.entity.vo.StoreDepartmentVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 店铺部门业务层
 *
 * @author Chopper
 * @since 2020/11/17 3:43 下午
 */
public interface StoreDepartmentService extends IService<StoreDepartment> {

    /**
     * 获取部门树
     *
     * @param initWrapper
     * @return
     */
    List<StoreDepartmentVO> tree(QueryWrapper<StoreDepartment> initWrapper);

    /**
     * 删除部门
     *
     * @param ids
     */
    void deleteByIds(List<String> ids);

    /**
     * 更新店铺部门
     *
     * @param storeDepartment 店铺部门
     * @return
     */
    Boolean update(StoreDepartment storeDepartment);
}