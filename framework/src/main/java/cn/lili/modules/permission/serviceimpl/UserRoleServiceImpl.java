package cn.lili.modules.permission.serviceimpl;

import cn.lili.modules.permission.entity.dos.UserRole;
import cn.lili.modules.permission.mapper.UserRoleMapper;
import cn.lili.modules.permission.service.UserRoleService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

/**
 * 用户权限业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 3:52 下午
 */
@Service
public class UserRoleServiceImpl extends ServiceImpl<UserRoleMapper, UserRole> implements UserRoleService {

    @Override
    public List<UserRole> listByUserId(String userId) {
        QueryWrapper<UserRole> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("user_id", userId);
        return this.baseMapper.selectList(queryWrapper);
    }

    @Override
    public List<String> listId(String userId) {
        List<UserRole> userRoleList = this.listByUserId(userId);
        List<String> strings = new ArrayList<>();
        userRoleList.forEach(item -> strings.add(item.getRoleId()));
        return strings;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateUserRole(String userId, List<UserRole> userRoles) {

        //删除
        QueryWrapper<UserRole> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("user_id", userId);
        this.remove(queryWrapper);

        //保存
        this.saveBatch(userRoles);
    }

}
