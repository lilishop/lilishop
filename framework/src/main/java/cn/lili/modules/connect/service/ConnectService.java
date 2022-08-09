package cn.lili.modules.connect.service;

import cn.lili.cache.CachePrefix;
import cn.lili.common.security.token.Token;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.connect.entity.dto.WechatMPLoginParams;
import cn.lili.modules.member.entity.dto.ConnectQueryDTO;
import com.baomidou.mybatisplus.extension.service.IService;

import javax.naming.NoPermissionException;
import java.util.List;

/**
 * 联合登陆接口
 *
 * @author Chopper
 */
public interface ConnectService extends IService<Connect> {


    /**
     * 联合登陆cookie 常量
     */
    String CONNECT_COOKIE = "CONNECT_COOKIE";
    /**
     * 联合登陆cookie 常量
     */
    String CONNECT_TYPE = "CONNECT_TYPE";

    /**
     * 联合登陆
     *
     * @param type     类型
     * @param unionid  unionid
     * @param longTerm 是否长时间有效
     * @param uuid     UUID
     * @return token
     * @throws NoPermissionException 不允许操作
     */
    Token unionLoginCallback(String type, String unionid, String uuid, boolean longTerm) throws NoPermissionException;

    /**
     * 联合登陆对象直接登录
     *
     * @param type     第三方登录类型
     * @param authUser 第三方登录返回封装类
     * @param uuid     用户uuid
     * @return token
     */
    Token unionLoginCallback(String type, ConnectAuthUser authUser, String uuid);

    /**
     * 绑定
     *
     * @param unionId
     * @param type
     * @return
     */
    void bind(String unionId, String type);

    /**
     * 解绑
     *
     * @param type
     */
    void unbind(String type);

    /**
     * 已绑定列表
     *
     * @return
     */
    List<String> bindList();


    /**
     * 联合登录缓存key生成
     * 这个方法返回的key从缓存中可以获取到redis中记录到会员信息，有效时间30分钟
     *
     * @param type 联合登陆类型
     * @param uuid 联合登陆uuid
     * @return 返回KEY
     */
    static String cacheKey(String type, String uuid) {
        return CachePrefix.CONNECT_AUTH.getPrefix() + type + uuid;
    }

    /**
     * app联合登录 回调
     *
     * @param authUser 登录对象
     * @param uuid     uuid
     * @return token
     */
    Token appLoginCallback(ConnectAuthUser authUser, String uuid);


    /**
     * 微信一键登录
     * 小程序自动登录 没有账户自动注册
     *
     * @param params 微信小程序登录参数
     * @return token
     */
    Token miniProgramAutoLogin(WechatMPLoginParams params);

    /**
     * 根据查询dto获取查询对象
     *
     * @param connectQueryDTO
     * @return
     */
    Connect queryConnect(ConnectQueryDTO connectQueryDTO);

    /**
     * 根据会员id删除记录
     *
     * @param userId 会员id
     */
    void deleteByMemberId(String userId);
}