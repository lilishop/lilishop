package cn.lili.modules.member.serviceimpl;

import cn.lili.common.security.context.UserContext;
import cn.lili.modules.member.entity.dos.StoreDepartmentRole;
import cn.lili.modules.member.mapper.StoreDepartmentRoleMapper;
import cn.lili.modules.member.service.StoreDepartmentRoleService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 部门角色业务层实现
 *
 * @author Chopper
 * @since 2020/11/22 12:08
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class StoreDepartmentRoleServiceImpl extends ServiceImpl<StoreDepartmentRoleMapper, StoreDepartmentRole> implements StoreDepartmentRoleService {


    @Override
    public List<StoreDepartmentRole> listByDepartmentId(String storeDepartmentId) {
        QueryWrapper queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("department_id", storeDepartmentId);
        return this.baseMapper.selectList(queryWrapper);
    }

    @Override
    public void updateByDepartmentId(String storeDepartmentId, List<StoreDepartmentRole> storeDepartmentRoles) {
        if (storeDepartmentRoles.size() > 0) {
            QueryWrapper queryWrapper = new QueryWrapper<>();
            queryWrapper.eq("department_id", storeDepartmentId);
            this.remove(queryWrapper);
            this.saveBatch(storeDepartmentRoles);
        }
    }

    @Override
    public void deleteByDepartment(List<String> ids) {

        QueryWrapper queryWrapper = new QueryWrapper<>();
        queryWrapper.in("department_id", ids);
        this.remove(queryWrapper);
    }
}