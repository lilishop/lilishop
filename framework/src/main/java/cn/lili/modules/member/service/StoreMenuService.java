package cn.lili.modules.member.service;

import cn.lili.modules.member.entity.dos.StoreMenu;
import cn.lili.modules.member.entity.vo.StoreMenuVO;
import cn.lili.modules.permission.entity.dto.MenuSearchParams;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.cache.annotation.CacheConfig;

import java.util.List;

/**
 * 店铺菜单权限业务层
 *
 * @author Chopper
 * @since 2020/11/17 3:45 下午
 */
@CacheConfig(cacheNames = "{store_menu}")
public interface StoreMenuService extends IService<StoreMenu> {

    /**
     * 通过用户的菜单权限
     *
     * @return
     */
    List<StoreMenuVO> findUserTree();

    /**
     * 通过用户id获取
     *
     * @param userId
     * @return
     */
    List<StoreMenu> findUserList(String userId);


    /**
     * 根据角色id获取菜单集合
     *
     * @param roleIds
     * @return
     */
    List<StoreMenu> findByRoleIds(String roleIds);

    /**
     * 树形结构
     *
     * @return
     */
    List<StoreMenuVO> tree();

    /**
     * 查询列表
     *
     * @param menuSearchParams
     * @return
     */
    List<StoreMenu> searchList(MenuSearchParams menuSearchParams);

    /**
     * 批量删除
     *
     * @param ids
     */
    void deleteIds(List<String> ids);

    /**
     * 添加更新菜单
     *
     * @param storeMenu 菜单数据
     * @return 是否成功
     */
    boolean saveOrUpdateMenu(StoreMenu storeMenu);

}
