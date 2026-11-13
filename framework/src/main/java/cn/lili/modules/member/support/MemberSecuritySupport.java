package cn.lili.modules.member.support;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.support.LoginFailLimitSupport;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.utils.UuidUtils;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * 客户账号安全、密码与注销业务支持类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Component
public class MemberSecuritySupport {

    @Autowired
    private Cache cache;

    @Autowired
    private LoginFailLimitSupport loginFailLimitSupport;

    @Autowired
    @Lazy
    private ConnectService connectService;

    public MemberSecuritySupport() {
    }

    public MemberSecuritySupport(Cache cache, LoginFailLimitSupport loginFailLimitSupport, ConnectService connectService) {
        this.cache = cache;
        this.loginFailLimitSupport = loginFailLimitSupport;
        this.connectService = connectService;
    }

    /**
     * 重置客户密码
     *
     * @param memberService 客户服务实例
     * @param ids           客户ID列表
     */
    public void resetPassword(MemberService memberService, List<String> ids) {
        String password = new BCryptPasswordEncoder().encode(StringUtils.md5("123456"));
        LambdaUpdateWrapper<Member> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.in(Member::getId, ids);
        lambdaUpdateWrapper.set(Member::getPassword, password);
        memberService.update(lambdaUpdateWrapper);
        memberService.revokeMemberTokens(ids);
        if (ids != null && !ids.isEmpty() && loginFailLimitSupport != null) {
            List<Member> members = memberService.listByIds(ids);
            if (members != null) {
                members.forEach(m -> loginFailLimitSupport.clearLoginFail(UserEnums.MEMBER, m.getUsername()));
            }
        }
    }

    /**
     * 修改密码
     *
     * @param memberService 客户服务实例
     * @param oldPassword   旧密码
     * @param newPassword   新密码
     * @return 修改后的客户对象
     */
    public Member modifyPass(MemberService memberService, String oldPassword, String newPassword) {
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        Member member = memberService.getById(tokenUser.getId());
        if (!new BCryptPasswordEncoder().matches(oldPassword, member.getPassword())) {
            throw new ServiceException(ResultCode.USER_OLD_PASSWORD_ERROR);
        }
        LambdaUpdateWrapper<Member> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(Member::getId, member.getId());
        lambdaUpdateWrapper.set(Member::getPassword, new BCryptPasswordEncoder().encode(newPassword));
        memberService.update(lambdaUpdateWrapper);
        memberService.revokeMemberTokens(Collections.singletonList(member.getId()));
        return member;
    }

    /**
     * 校验当前登录用户是否处于默认初始化密码状态
     *
     * @param memberService 客户服务实例
     * @return 是否可初始化密码
     */
    public boolean canInitPass(MemberService memberService) {
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        Member member = memberService.getById(tokenUser.getId());
        return member != null && MemberService.DEFAULT_PASSWORD.equals(member.getPassword());
    }

    /**
     * 初始化客户密码
     *
     * @param memberService 客户服务实例
     * @param password      新密码
     */
    public void initPass(MemberService memberService, String password) {
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        Member member = memberService.getById(tokenUser.getId());
        if (member != null && MemberService.DEFAULT_PASSWORD.equals(member.getPassword())) {
            LambdaUpdateWrapper<Member> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
            lambdaUpdateWrapper.eq(Member::getId, member.getId());
            lambdaUpdateWrapper.set(Member::getPassword, new BCryptPasswordEncoder().encode(password));
            memberService.update(lambdaUpdateWrapper);
            memberService.revokeMemberTokens(Collections.singletonList(member.getId()));
            return;
        }
        throw new ServiceException(ResultCode.UNINITIALIZED_PASSWORD);
    }

    /**
     * 注销当前登录客户账号
     *
     * @param memberService 客户服务实例
     */
    public void cancellation(MemberService memberService) {
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        Member member = memberService.getById(tokenUser.getId());
        if (member != null) {
            if (connectService != null) {
                connectService.deleteByMemberId(member.getId());
            }
            confusionMember(memberService, member);
        }
    }

    /**
     * 混淆注销客户个人敏感信息
     *
     * @param memberService 客户服务实例
     * @param member        客户对象
     */
    public void confusionMember(MemberService memberService, Member member) {
        member.setUsername(UuidUtils.getUUID());
        member.setMobile(UuidUtils.getUUID() + member.getMobile());
        member.setNickName("用户已注销");
        member.setDisabled(false);
        memberService.updateById(member);
    }

    /**
     * 修改当前登录客户手机号
     *
     * @param memberService 客户服务实例
     * @param mobile        新手机号
     * @return 是否修改成功
     */
    public boolean changeMobile(MemberService memberService, String mobile) {
        AuthUser tokenUser = Objects.requireNonNull(UserContext.getCurrentUser());
        Member member = memberService.findByUsername(tokenUser.getUsername());
        if (member == null || !Objects.equals(tokenUser.getId(), member.getId())) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        return changeMobile(memberService, member.getId(), mobile);
    }

    /**
     * 修改指定客户手机号
     *
     * @param memberService 客户服务实例
     * @param memberId      客户ID
     * @param mobile        新手机号
     * @return 是否修改成功
     */
    public boolean changeMobile(MemberService memberService, String memberId, String mobile) {
        LambdaUpdateWrapper<Member> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(Member::getId, memberId);
        lambdaUpdateWrapper.set(Member::getMobile, mobile);
        return memberService.update(lambdaUpdateWrapper);
    }

    /**
     * 根据手机验证码凭据重置密码
     *
     * @param memberService 客户服务实例
     * @param uuid          验证凭证UUID
     * @param password      新密码
     * @return 是否重置成功
     */
    public boolean resetByMobile(MemberService memberService, String uuid, String password) {
        Object phoneObj = cache.get(CachePrefix.FIND_MOBILE + uuid);
        if (phoneObj == null) {
            throw new ServiceException(ResultCode.USER_PHONE_NOT_EXIST);
        }
        String phone = phoneObj.toString();
        Member member = memberService.findByMobile(phone);
        if (member == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        LambdaUpdateWrapper<Member> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(Member::getMobile, phone);
        lambdaUpdateWrapper.set(Member::getPassword, new BCryptPasswordEncoder().encode(password));
        cache.remove(CachePrefix.FIND_MOBILE + uuid);
        boolean updated = memberService.update(lambdaUpdateWrapper);
        if (updated) {
            memberService.revokeMemberTokens(Collections.singletonList(member.getId()));
            if (loginFailLimitSupport != null) {
                loginFailLimitSupport.clearLoginFail(UserEnums.MEMBER, member.getUsername());
            }
        }
        return updated;
    }
}
