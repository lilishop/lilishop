package cn.lili.modules.system.token;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.PermissionEnum;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.token.Token;
import cn.lili.common.security.token.TokenUtil;
import cn.lili.common.security.token.base.AbstractTokenGenerate;
import cn.lili.modules.permission.entity.dos.AdminUser;
import cn.lili.modules.permission.entity.vo.UserMenuVO;
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
 * @since 2020/11/16 10:51
 */
@Component
public class ManagerTokenGenerate extends AbstractTokenGenerate<AdminUser> {

    @Autowired
    private TokenUtil tokenUtil;
    @Autowired
    private RoleMenuService roleMenuService;
    @Autowired
    private Cache cache;


    @Override
    public Token createToken(AdminUser adminUser, Boolean longTerm) {
        AuthUser authUser = new AuthUser(adminUser.getUsername(), adminUser.getId(), adminUser.getAvatar(), UserEnums.MANAGER, adminUser.getNickName(), adminUser.getIsSuper());

        List<UserMenuVO> userMenuVOList = roleMenuService.findAllMenu(authUser.getId());
        //缓存权限列表
        cache.put(CachePrefix.PERMISSION_LIST.getPrefix(UserEnums.MANAGER) + authUser.getId(), this.permissionList(userMenuVOList));

        return tokenUtil.createToken(adminUser.getUsername(), authUser, longTerm, UserEnums.MANAGER);
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

        List<String> superPermissions = new ArrayList<>();
        List<String> queryPermissions = new ArrayList<>();
        initPermission(superPermissions, queryPermissions);

        //循环权限菜单
        if (userMenuVOList != null && !userMenuVOList.isEmpty()) {
            userMenuVOList.forEach(menu -> {
                //循环菜单，赋予用户权限
                if (CharSequenceUtil.isNotEmpty(menu.getPermission())) {
                    //获取路径集合
                    String[] permissionUrl = menu.getPermission().split(",");
                    //for循环路径集合
                    for (String url : permissionUrl) {
                        //如果是超级权限 则计入超级权限
                        if (Boolean.TRUE.equals(menu.getSuper())) {
                            //如果已有超级权限，则这里就不做权限的累加
                            if (!superPermissions.contains(url)) {
                                superPermissions.add(url);
                            }
                        }
                        //否则计入浏览权限
                        else {
                            //没有权限，则累加。
                            if (!queryPermissions.contains(url)) {
                                queryPermissions.add(url);
                            }
                        }
                    }
                }
                //去除重复的权限
                queryPermissions.removeAll(superPermissions);
            });
        }
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
        //TODO 用户信息维护--操作权限
        //获取当前登录用户
        superPermissions.add("/manager/passport/user/info*");
        //修改用户资料
        superPermissions.add("/manager/passport/user/edit*");
        //修改密码
        superPermissions.add("/manager/passport/user/editPassword*");
        //退出
        superPermissions.add("/manager/passport/user/logout*");

        //统计查看权限
        queryPermissions.add("/manager/statistics*");
        //菜单查看权限
        queryPermissions.add("/manager/permission/menu*");
        //商品分类查看权限
        queryPermissions.add("/manager/goods/category*");
        //查看地区接口
        queryPermissions.add("/manager/setting/region*");

    }

}
