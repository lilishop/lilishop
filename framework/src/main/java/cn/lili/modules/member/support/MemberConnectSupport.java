package cn.lili.modules.member.support;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.CommonUtil;
import cn.lili.common.utils.CookieUtil;
import cn.lili.common.utils.UuidUtils;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.connect.support.ConnectBindingSupport;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.ConnectQueryDTO;
import cn.lili.modules.member.service.MemberService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.function.Consumer;

/**
 * 客户第三方联合登录与绑定支持类
 * 绑定记录通过独立存储组件读写，不反向调用联合登录业务服务。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class MemberConnectSupport {

    private static final String WECHAT_DEFAULT_NICKNAME = "微信用户";
    private static final int NICKNAME_SUFFIX_LENGTH = 4;

    @Autowired
    private ConnectBindingSupport connectBindings;

    @Autowired
    private Cache cache;

    public MemberConnectSupport() {
    }

    /**
     * 注入绑定基础读写和认证上下文缓存。
     *
     * @param connectBindings 联合登录绑定存储组件
     * @param cache 联合登录认证上下文缓存
     */
    public MemberConnectSupport(ConnectBindingSupport connectBindings, Cache cache) {
        this.connectBindings = connectBindings;
        this.cache = cache;
    }

    /**
     * 联合登录自动注册新会员
     *
     * @param authUser        联合登录认证对象
     * @param registerHandler 注册处理器回调
     * @return 注册成功的会员对象
     */
    public Member autoRegister(ConnectAuthUser authUser, Consumer<Member> registerHandler) {
        if (CharSequenceUtil.isEmpty(authUser.getNickname())) {
            authUser.setNickname(CommonUtil.getSpecialStr("用户"));
        } else if (WECHAT_DEFAULT_NICKNAME.equals(authUser.getNickname().trim())) {
            authUser.setNickname(WECHAT_DEFAULT_NICKNAME + openIdSuffix(authUser.getUuid()));
        }
        if (CharSequenceUtil.isEmpty(authUser.getAvatar())) {
            authUser.setAvatar("https://i.loli.net/2020/11/19/LyN6JF7zZRskdIe.png");
        }
        try {
            Member member = new Member(authUser.getUsername(), UuidUtils.getUUID(), authUser.getAvatar(), authUser.getNickname(),
                    authUser.getGender() != null ? Convert.toInt(authUser.getGender().getCode()) : 0, authUser.getPhone());
            member.setPassword(MemberService.DEFAULT_PASSWORD);
            if (registerHandler != null) {
                registerHandler.accept(member);
            }
            return member;
        } catch (ServiceException e) {
            log.error("自动注册服务抛出异常：", e);
            throw e;
        } catch (Exception e) {
            log.error("自动注册异常：", e);
            throw new ServiceException(ResultCode.USER_AUTO_REGISTER_ERROR);
        }
    }

    /**
     * 成功登录，则检测cookie中的信息，进行客户绑定
     *
     * @param member 客户
     */
    public void loginBindUser(Member member) {
        // 获取cookie存储的信息
        String uuid = CookieUtil.getCookie(ConnectService.CONNECT_COOKIE, ThreadContextHolder.getHttpRequest());
        String connectType = CookieUtil.getCookie(ConnectService.CONNECT_TYPE, ThreadContextHolder.getHttpRequest());
        // 如果联合登录存储了信息
        if (CharSequenceUtil.isNotEmpty(uuid) && CharSequenceUtil.isNotEmpty(connectType)) {
            try {
                ConnectAuthUser connectAuthUser = getConnectAuthUser(uuid, connectType);
                if (connectAuthUser == null) {
                    return;
                }
                Connect connect = connectBindings.queryConnect(
                        ConnectQueryDTO.builder().unionId(connectAuthUser.getUuid()).unionType(connectType).build()
                );
                if (connect == null) {
                    connect = new Connect(member.getId(), connectAuthUser.getUuid(), connectType);
                    connectBindings.save(connect);
                }
            } catch (ServiceException e) {
                throw e;
            } catch (Exception e) {
                log.error("绑定第三方联合登录失败：", e);
            } finally {
                // 联合登录成功与否，都清除掉cookie中的信息
                CookieUtil.delCookie(ConnectService.CONNECT_COOKIE, ThreadContextHolder.getHttpResponse());
                CookieUtil.delCookie(ConnectService.CONNECT_TYPE, ThreadContextHolder.getHttpResponse());
            }
        }
    }

    /**
     * 获取cookie中的联合登录对象
     *
     * @param uuid uuid
     * @param type 联合登录类型
     * @return 联合登录认证对象
     */
    public ConnectAuthUser getConnectAuthUser(String uuid, String type) {
        Object context = cache.get(ConnectService.cacheKey(type, uuid));
        if (context != null) {
            return (ConnectAuthUser) context;
        }
        return null;
    }

    /**
     * 取 openid 后四位作为昵称后缀，openid 缺失时使用随机数兜底
     *
     * @param openId 微信openid
     * @return 昵称后缀
     */
    public String openIdSuffix(String openId) {
        if (CharSequenceUtil.isEmpty(openId)) {
            return CommonUtil.getRandomNum();
        }
        return openId.length() > NICKNAME_SUFFIX_LENGTH
                ? openId.substring(openId.length() - NICKNAME_SUFFIX_LENGTH)
                : openId;
    }
}
