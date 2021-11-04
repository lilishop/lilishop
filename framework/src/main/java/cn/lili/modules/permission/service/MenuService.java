package cn.lili.modules.permission.service;

import cn.lili.modules.permission.entity.dos.Menu;
import cn.lili.modules.permission.entity.dto.MenuSearchParams;
import cn.lili.modules.permission.entity.vo.MenuVO;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.cache.annotation.CacheConfig;

import java.util.List;

/**
 * 权限业务层
 *
 * @author Chopper
 * @since 2020/11/17 3:45 下午
 */
@CacheConfig(cacheNames = "{menu}")
public interface MenuService extends IService<Menu> {

    /**
     * 通过用户的菜单权限
     *
     * @return
     */
    List<MenuVO> findUserTree();

    /**
     * 通过用户id获取
     *
     * @param userId
     * @return
     */
    List<Menu> findUserList(String userId);


    /**
     * 根据角色id获取菜单集合
     *
     * @param roleIds
     * @return
     */
    List<Menu> findByRoleIds(String roleIds);

    /**
     * 树形结构
     *
     * @return
     */
    List<MenuVO> tree();

    /**
     * 查询列表
     *
     * @param menuSearchParams
     * @return
     */
    List<Menu> searchList(MenuSearchParams menuSearchParams);

    /**
     * 批量删除
     *
     * @param ids
     */
    void deleteIds(List<String> ids);

    /**
     * 添加更新菜单
     *
     * @param menu 菜单数据
     * @return 是否成功
     */
    boolean saveOrUpdateMenu(Menu menu);

}
