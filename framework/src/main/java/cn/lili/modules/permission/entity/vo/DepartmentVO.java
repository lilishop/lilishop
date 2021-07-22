package cn.lili.modules.permission.entity.vo;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.permission.entity.dos.Department;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

/**
 * 部门VO
 *
 * @author Chopper
 * @since 2020-11-23 18:48
 */
@Data
public class DepartmentVO extends Department {

    private List<DepartmentVO> children = new ArrayList<>();

    public DepartmentVO() {
    }

    public DepartmentVO(Department department) {
        BeanUtil.copyProperties(department, this);
    }
}
