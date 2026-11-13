package cn.lili.modules.member.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.member.entity.dos.Clerk;
import cn.lili.modules.member.entity.dos.StoreDepartment;
import cn.lili.modules.member.entity.vo.StoreDepartmentVO;
import cn.lili.modules.member.mapper.StoreDepartmentMapper;
import cn.lili.modules.member.mapper.ClerkMapper;
import cn.lili.modules.member.service.StoreDepartmentRoleService;
import cn.lili.modules.member.service.StoreDepartmentService;
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
 * 店铺部门业务层实现
 * 删除前通过员工存储检查部门占用，不反向依赖员工业务服务。
 *
 * @author Chopper
 * @since 2020/11/17 3:47 下午
 */
@Slf4j
@Service
@Transactional(rollbackFor = Exception.class)
public class StoreDepartmentServiceImpl extends ServiceImpl<StoreDepartmentMapper, StoreDepartment> implements StoreDepartmentService {

    @Autowired
    private StoreDepartmentRoleService storeDepartmentRoleService;

    @Autowired
    private ClerkMapper clerkMapper;

    /**
     * 校验全部部门属于当前店铺且未绑定员工后，在同一事务内删除部门及部门角色关联。
     * 员工占用查询沿用原部门ID范围和存储层逻辑删除规则，不改变角色服务的缓存失效处理。
     *
     * @param ids 待删除的店铺部门主键列表
     * @throws ServiceException 存在其他店铺部门或部门已绑定员工时抛出
     */
    @Override
    public void deleteByIds(List<String> ids) {
        //校验是否有操作店铺部门权限
        List<StoreDepartment> storeDepartments = this.list(new QueryWrapper<StoreDepartment>()
                .in("id", ids)
                .eq("store_id", UserContext.getCurrentUser().getStoreId()));
        if (storeDepartments.size() != ids.size()) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        //校验店员是否绑定部门
        if (clerkMapper.selectCount(new QueryWrapper<Clerk>().in("department_id", ids)) > 0) {
            throw new ServiceException(ResultCode.PERMISSION_DEPARTMENT_DELETE_ERROR);
        }
        //删除店铺部门
        this.removeByIds(ids);
        //删除店铺部门角色
        storeDepartmentRoleService.deleteByDepartment(ids);
    }

    @Override
    public List<StoreDepartmentVO> tree(QueryWrapper<StoreDepartment> initWrapper) {
        try {
            List<StoreDepartment> departments = this.list(initWrapper);

            List<StoreDepartmentVO> all = new ArrayList<>();
            departments.forEach(item -> all.add(new StoreDepartmentVO(item)));

            List<StoreDepartmentVO> tree = new ArrayList<>();
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
    public List<StoreDepartmentVO> tree(StoreDepartment entity, SearchVO searchVO) {
        return tree(PageUtil.initWrapper(entity, searchVO));
    }


    /**
     * 递归初始化子树
     *
     * @param tree          树结构
     * @param departmentVOS 数据库对象集合
     */
    private void initChild(StoreDepartmentVO tree, List<StoreDepartmentVO> departmentVOS) {
        departmentVOS.stream()
                .filter(item -> (item.getParentId().equals(tree.getId())))
                .forEach(child -> {
                    StoreDepartmentVO childTree = new StoreDepartmentVO(child);
                    initChild(childTree, departmentVOS);
                    tree.getChildren().add(childTree);
                });
    }

    @Override
    public Boolean update(StoreDepartment storeDepartment) {
        StoreDepartment temp = this.getById(storeDepartment);
        //校验部门是否存在
        if (temp == null) {
            throw new ServiceException(ResultCode.PERMISSION_NOT_FOUND_ERROR);
        }
        //校验店铺权限
        if (!temp.getStoreId().equals(UserContext.getCurrentUser().getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        return this.updateById(storeDepartment);
    }
}
