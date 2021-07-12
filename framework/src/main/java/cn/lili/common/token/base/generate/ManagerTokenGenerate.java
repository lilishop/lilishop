package cn.lili.common.token.base.generate;

import cn.lili.common.cache.Cache;
import cn.lili.common.cache.CachePrefix;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.token.PermissionEnum;
import cn.lili.common.token.Token;
import cn.lili.common.token.TokenUtil;
import cn.lili.common.token.base.AbstractTokenGenerate;
import cn.lili.modules.permission.entity.dos.AdminUser;
import cn.lili.modules.permission.entity.vo.UserMenuVO;
import cn.lili.modules.permission.service.AdminUserService;
import cn.lili.modules.permission.service.RoleMenuService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 管理员token生成
 *
 * @author Chopper
 * @version v4.0
 * @Description:
 * @since 2020/11/16 10:51
 */
@Component
public class ManagerTokenGenerate extends AbstractTokenGenerate {

    @Autowired
    private AdminUserService adminUserService;
    @Autowired
    private TokenUtil tokenUtil;
    @Autowired
    private RoleMenuService roleMenuService;
    @Autowired
    private Cache cache;


    @Override
    public Token createToken(String username, Boolean longTerm) {
        //生成token
        AdminUser adminUser = adminUserService.findByUsername(username);
        AuthUser user = new AuthUser(adminUser.getUsername(), adminUser.getId(), UserEnums.MANAGER, adminUser.getNickName(), adminUser.getIsSuper());


        List<UserMenuVO> userMenuVOList = roleMenuService.findAllMenu(user.getId());
        //缓存权限列表
        cache.put(CachePrefix.PERMISSION_LIST.getPrefix(UserEnums.MANAGER) + user.getId(), this.permissionList(userMenuVOList));

        return tokenUtil.createToken(username, user, longTerm, UserEnums.MANAGER);
    }

    @Override
    public Token refreshToken(String refreshToken) {
        return tokenUtil.refreshToken(refreshToken, UserEnums.MANAGER);
    }

    /**
     * 获取用户权限
     *
     * @param userMenuVOList
     * @return
     */
    private Map<String, List<String>> permissionList(List<UserMenuVO> userMenuVOList) {
        Map<String, List<String>> permission = new HashMap<>(2);
        if (userMenuVOList == null || userMenuVOList.size() == 0) {
            return permission;
        }
        List<String> superPermissions = new ArrayList<>();
        List<String> queryPermissions = new ArrayList<>();
        initPermission(superPermissions, queryPermissions);

        //循环权限菜单
        userMenuVOList.forEach(menu -> {
            //循环菜单，赋予用户权限
            if (menu.getPath() != null) {
                //获取路径集合
                String[] paths = menu.getPath().split("\\|");
                //for循环路径集合
                for (String path : paths) {
                    //如果是超级权限 则计入超级权限
                    if (menu.getIsSupper() != null && menu.getIsSupper()) {
                        //如果已有超级权限，则这里就不做权限的累加
                        if (!superPermissions.contains(path)) {
                            superPermissions.add(path);
                        }
                    }
                    //否则计入浏览权限
                    else {
                        //如果已有超级权限，或者已有普通查看权限，则这里就不做权限的累加
                        if (!superPermissions.contains(path) && !queryPermissions.contains(path)) {
                            queryPermissions.add(path);
                        }
                    }
                }
            }

            //去除无效的权限
            superPermissions.forEach(queryPermissions::remove);
        });
        permission.put(PermissionEnum.SUPER.name(), superPermissions);
        permission.put(PermissionEnum.QUERY.name(), queryPermissions);
        return permission;
    }

    /**
     * 初始赋予的权限，查看权限包含首页流量统计权限，
     * 超级权限包含个人信息维护，密码修改权限
     *
     * @param superPermissions 超级权限
     * @param queryPermissions 查询权限
     */
    void initPermission(List<String> superPermissions, List<String> queryPermissions) {
        //用户信息维护
        superPermissions.add("/manager/user/info");
        superPermissions.add("/manager/user/edit");
        superPermissions.add("/manager/user/editPassword*");
        //统计查看
        queryPermissions.add("/manager/statistics*");
    }

}
