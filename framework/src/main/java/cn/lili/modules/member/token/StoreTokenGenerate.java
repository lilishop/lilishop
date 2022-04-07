package cn.lili.modules.member.token;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.PermissionEnum;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.token.Token;
import cn.lili.common.security.token.TokenUtil;
import cn.lili.common.security.token.base.AbstractTokenGenerate;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.StoreService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import cn.lili.modules.member.entity.dos.Clerk;
import cn.lili.modules.member.entity.vo.StoreUserMenuVO;
import cn.lili.modules.member.service.ClerkService;
import cn.lili.modules.member.service.StoreMenuRoleService;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 商家token生成
 *
 * @author Chopper
 * @version v4.0
 * @since 2020/11/16 10:51
 */
@Component
public class StoreTokenGenerate extends AbstractTokenGenerate<Member> {
    @Autowired
    private StoreService storeService;
    @Autowired
    private TokenUtil tokenUtil;
    @Autowired
    private StoreMenuRoleService storeMenuRoleService;
    @Autowired
    private Cache cache;
    @Autowired
    private ClerkService clerkService;

    @Override
    public Token createToken(Member member, Boolean longTerm) {
        if (Boolean.FALSE.equals(member.getHaveStore())) {
            throw new ServiceException(ResultCode.STORE_NOT_OPEN);
        }
        //根据会员id查询店员信息
        Clerk clerk = clerkService.getClerkByMemberId(member.getId());

        if (clerk == null) {
            throw new ServiceException(ResultCode.CLERK_NOT_FOUND_ERROR);
        }
        if (!clerk.getStatus()) {
            throw new ServiceException(ResultCode.CLERK_DISABLED_ERROR);
        }
        //获取当前用户权限
        List<StoreUserMenuVO> storeUserMenuVOS = storeMenuRoleService.findAllMenu(clerk.getId(),member.getId());
        //缓存权限列表
        cache.put(CachePrefix.PERMISSION_LIST.getPrefix(UserEnums.STORE) + member.getId(), this.permissionList(storeUserMenuVOS));
        //查询店铺信息
        Store store = storeService.getById(clerk.getStoreId());
        if (store == null) {
            throw new ServiceException(ResultCode.STORE_NOT_OPEN);
        }
        AuthUser authUser = new AuthUser(member.getUsername(), member.getId(), UserEnums.STORE, member.getNickName(), clerk.getIsSuper(), clerk.getId(),store.getStoreLogo());

        authUser.setStoreId(store.getId());
        authUser.setStoreName(store.getStoreName());
        return tokenUtil.createToken(member.getUsername(), authUser, longTerm, UserEnums.STORE);
    }

    @Override
    public Token refreshToken(String refreshToken) {
        return tokenUtil.refreshToken(refreshToken, UserEnums.STORE);
    }

    /**
     * 获取用户权限
     *
     * @param userMenuVOList
     * @return
     */
    private Map<String, List<String>> permissionList(List<StoreUserMenuVO> userMenuVOList) {
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
        //菜单管理
        superPermissions.add("/store/menu*");
        //退出权限
        superPermissions.add("/store/passport/login/logout*");
        //修改
        superPermissions.add("/store/passport/login*");



        //店铺设置
        queryPermissions.add("/store/settings/storeSettings*");
        //文章接口
        queryPermissions.add("/store/other/article*");
        //首页统计
        queryPermissions.add("/store/statistics/index*");



    }
}
