package cn.lili.modules.member.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.member.entity.dos.Clerk;
import cn.lili.modules.member.entity.dos.StoreMenu;
import cn.lili.modules.member.entity.dos.StoreMenuRole;
import cn.lili.modules.member.entity.vo.StoreMenuVO;
import cn.lili.modules.member.mapper.StoreMenuMapper;
import cn.lili.modules.member.service.ClerkService;
import cn.lili.modules.member.service.StoreMenuRoleService;
import cn.lili.modules.member.service.StoreMenuService;
import cn.lili.modules.permission.entity.dto.MenuSearchParams;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * 权限业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 3:49 下午
 */
@Slf4j
@Service
public class StoreMenuServiceImpl extends ServiceImpl<StoreMenuMapper, StoreMenu> implements StoreMenuService {
    /**
     * 菜单角色
     */
    @Autowired
    private StoreMenuRoleService storeMenuRoleService;

    @Autowired
    private Cache<List<StoreMenu>> cache;

    /**
     * 店员
     */
    @Autowired
    private ClerkService clerkService;

    @Override
    public void deleteIds(List<String> ids) {
        QueryWrapper<StoreMenuRole> queryWrapper = new QueryWrapper<>();
        queryWrapper.in("menu_id", ids);
        //如果已有角色绑定菜单，则不能直接删除
        if (storeMenuRoleService.count(queryWrapper) > 0) {
            throw new ServiceException(ResultCode.PERMISSION_MENU_ROLE_ERROR);
        }
        this.removeByIds(ids);
    }


    @Override
    public List<StoreMenuVO> findUserTree() {
        AuthUser authUser = Objects.requireNonNull(UserContext.getCurrentUser());
        if (Boolean.TRUE.equals(authUser.getIsSuper())) {
            return this.tree();
        }
        //获取当前登录用户的店员信息
        Clerk clerk = clerkService.getOne(new LambdaQueryWrapper<Clerk>().eq(Clerk::getMemberId, authUser.getId()));
        //获取当前店员角色的菜单列表
        List<StoreMenu> userMenus = this.baseMapper.findByUserId(clerk.getId());
        return this.tree(userMenus);
    }

    @Override
    public List<StoreMenu> findUserList(String userId) {
        String cacheKey = CachePrefix.STORE_MENU_USER_ID.getPrefix() + userId;
        List<StoreMenu> menuList = cache.get(cacheKey);
        if (menuList == null) {
            menuList = this.baseMapper.findByUserId(userId);
            cache.put(cacheKey, menuList);
        }
        return menuList;
    }

    /**
     * 添加更新菜单
     *
     * @param storeMenu 菜单数据
     * @return 是否成功
     */
    @Override
    public boolean saveOrUpdateMenu(StoreMenu storeMenu) {
        if (CharSequenceUtil.isNotEmpty(storeMenu.getId())) {
            cache.vagueDel(CachePrefix.STORE_MENU_USER_ID.getPrefix());
        }
        return this.saveOrUpdate(storeMenu);
    }

    @Override
    public List<StoreMenu> findByRoleIds(String roleId) {
        QueryWrapper<StoreMenu> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("role_id", roleId);
        return this.list(queryWrapper);
    }

    @Override
    public List<StoreMenu> searchList(MenuSearchParams menuSearchParams) {
        //title 需要特殊处理
        String title = null;
        if (CharSequenceUtil.isNotEmpty(menuSearchParams.getTitle())) {
            title = menuSearchParams.getTitle();
            menuSearchParams.setTitle(null);
        }
        QueryWrapper<StoreMenu> queryWrapper = PageUtil.initWrapper(menuSearchParams, new SearchVO());
        if (CharSequenceUtil.isNotEmpty(title)) {
            queryWrapper.like("title", title);
        }
        queryWrapper.orderByDesc("sort_order");
        return this.baseMapper.selectList(queryWrapper);
    }


    @Override
    public List<StoreMenuVO> tree() {
        try {
            List<StoreMenu> menus = this.list();
            return tree(menus);
        } catch (Exception e) {
            log.error("菜单树错误", e);
        }
        return Collections.emptyList();
    }

    /**
     * 传入自定义菜单集合
     *
     * @param menus 自定义菜单集合
     * @return 修改后的自定义菜单集合
     */
    private List<StoreMenuVO> tree(List<StoreMenu> menus) {
        List<StoreMenuVO> tree = new ArrayList<>();
        menus.forEach(item -> {
            if (item.getLevel() == 0) {
                StoreMenuVO treeItem = new StoreMenuVO(item);
                initChild(treeItem, menus);
                tree.add(treeItem);
            }
        });
        //对一级菜单排序
        tree.sort(Comparator.comparing(StoreMenu::getSortOrder));
        return tree;
    }

    /**
     * 递归初始化子树
     *
     * @param tree  树结构
     * @param menus 数据库对象集合
     */
    private void initChild(StoreMenuVO tree, List<StoreMenu> menus) {
        if (menus == null) {
            return;
        }
        menus.stream()
                .filter(item -> (item.getParentId().equals(tree.getId())))
                .forEach(child -> {
                    StoreMenuVO childTree = new StoreMenuVO(child);
                    initChild(childTree, menus);
                    tree.getChildren().add(childTree);
                });
    }

}
