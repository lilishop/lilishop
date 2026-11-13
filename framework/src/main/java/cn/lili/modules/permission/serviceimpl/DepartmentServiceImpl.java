package cn.lili.modules.permission.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.permission.entity.dos.AdminUser;
import cn.lili.modules.permission.entity.dos.Department;
import cn.lili.modules.permission.entity.vo.DepartmentVO;
import cn.lili.modules.permission.mapper.DepartmentMapper;
import cn.lili.modules.permission.mapper.AdminUserMapper;
import cn.lili.modules.permission.service.DepartmentRoleService;
import cn.lili.modules.permission.service.DepartmentService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

/**
 * 部门业务层实现
 * 删除前通过管理员存储检查部门占用，避免与管理员服务相互依赖。
 *
 * @author Chopper
 * @since 2020/11/17 3:47 下午
 */
@Slf4j
@Service
public class DepartmentServiceImpl extends ServiceImpl<DepartmentMapper, Department> implements DepartmentService {

    @Autowired
    private AdminUserMapper adminUserMapper;

    @Autowired
    private DepartmentRoleService departmentRoleService;

    /**
     * 部门下存在管理员时禁止删除，否则在同一事务内删除部门及部门角色关联。
     * 管理员占用查询沿用原有部门ID范围和存储层逻辑删除规则，保留角色服务的缓存失效处理。
     *
     * @param ids 待删除的部门主键列表
     * @throws ServiceException 部门下存在管理员时抛出
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteByIds(List<String> ids) {
        QueryWrapper<AdminUser> queryWrapper = new QueryWrapper<>();
        queryWrapper.in("department_id", ids);
        if (adminUserMapper.selectCount(queryWrapper) > 0) {
            throw new ServiceException(ResultCode.PERMISSION_DEPARTMENT_DELETE_ERROR);
        }
        this.removeByIds(ids);
        departmentRoleService.deleteByDepartment(ids);
    }

    @Override
    public List<DepartmentVO> tree(QueryWrapper<Department> initWrapper) {
        try {
            List<Department> departments = this.list(initWrapper);

            List<DepartmentVO> all = new ArrayList<>();
            departments.forEach(item -> all.add(new DepartmentVO(item)));

            List<DepartmentVO> tree = new ArrayList<>();
            all.forEach(item -> {
                if ("0".equals(item.getParentId())) {
                    initChild(item, all);
                    tree.add(item);
                }
            });

            return tree;
        } catch (Exception e) {
            log.error("部门业务错误", e);
            return null;
        }
    }

    @Override
    public List<DepartmentVO> tree(Department entity, SearchVO searchVO) {
        return this.tree(PageUtil.initWrapper(entity, searchVO));
    }


    /**
     * 递归初始化子树
     *
     * @param tree          树结构
     * @param departmentVOS 数据库对象集合
     */
    private void initChild(DepartmentVO tree, List<DepartmentVO> departmentVOS) {
        departmentVOS.stream()
                .filter(item -> (item.getParentId().equals(tree.getId())))
                .forEach(child -> {
                    DepartmentVO childTree = new DepartmentVO(child);
                    initChild(childTree, departmentVOS);
                    tree.getChildren().add(childTree);
                });
    }


}
