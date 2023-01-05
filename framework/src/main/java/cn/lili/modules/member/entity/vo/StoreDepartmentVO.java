package cn.lili.modules.member.entity.vo;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.member.entity.dos.StoreDepartment;
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
public class StoreDepartmentVO extends StoreDepartment {

    private List<StoreDepartmentVO> children = new ArrayList<>();

    public StoreDepartmentVO() {
    }

    public StoreDepartmentVO(StoreDepartment storeDepartment) {
        BeanUtil.copyProperties(storeDepartment, this);
    }
}
