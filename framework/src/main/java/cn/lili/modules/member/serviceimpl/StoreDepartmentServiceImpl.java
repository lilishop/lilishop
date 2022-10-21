package cn.lili.modules.member.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.member.entity.dos.Clerk;
import cn.lili.modules.member.entity.dos.StoreDepartment;
import cn.lili.modules.member.entity.vo.StoreDepartmentVO;
import cn.lili.modules.member.mapper.StoreDepartmentMapper;
import cn.lili.modules.member.service.ClerkService;
import cn.lili.modules.member.service.StoreDepartmentRoleService;
import cn.lili.modules.member.service.StoreDepartmentService;
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
    private ClerkService clerkService;

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
        if (clerkService.count(new QueryWrapper<Clerk>().in("department_id", ids)) > 0) {
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