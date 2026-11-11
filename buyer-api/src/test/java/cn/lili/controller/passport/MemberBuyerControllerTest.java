package cn.lili.controller.passport;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.sms.SmsUtil;
import cn.lili.modules.verification.entity.enums.VerificationEnums;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

/**
 * 买家端会员控制器测试用例，重点验证换绑手机接口鉴权及安全性
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@ExtendWith(MockitoExtension.class)
class MemberBuyerControllerTest {

    @Mock
    private MemberService memberService;

    @Mock
    private SmsUtil smsUtil;

    @InjectMocks
    private MemberBuyerController memberBuyerController;

    private MockedStatic<UserContext> userContextMock;

    @BeforeEach
    void setUp() {
        userContextMock = mockStatic(UserContext.class);
    }

    @AfterEach
    void tearDown() {
        if (userContextMock != null) {
            userContextMock.close();
        }
    }

    /**
     * 未登录状态调用换绑手机接口时应抛出用户未登录异常
     */
    @Test
    @DisplayName("换绑手机-未登录拦截")
    void bindMobileThrowsWhenNotLogin() {
        userContextMock.when(UserContext::getCurrentUser).thenReturn(null);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                memberBuyerController.bindMobile("13800138000", "123456", "uuid-123"));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_NOT_LOGIN);
        verify(smsUtil, never()).verifyCode(any(), any(), any(), any());
    }

    /**
     * 短信验证码校验失败时应抛出验证码错误异常
     */
    @Test
    @DisplayName("换绑手机-验证码校验失败")
    void bindMobileThrowsWhenSmsCodeInvalid() {
        AuthUser authUser = new AuthUser("testUser", "1001", "member", "test", UserEnums.MEMBER);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);

        when(smsUtil.verifyCode("13800138000", VerificationEnums.BIND_MOBILE, "uuid-123", "999999"))
                .thenReturn(false);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                memberBuyerController.bindMobile("13800138000", "999999", "uuid-123"));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.VERIFICATION_SMS_CHECKED_ERROR);
        verify(memberService, never()).changeMobile(any(), any());
    }

    /**
     * 当前登录用户在数据库中不存在时抛出用户不存在异常
     */
    @Test
    @DisplayName("换绑手机-当前用户不存在")
    void bindMobileThrowsWhenUserNotExist() {
        AuthUser authUser = new AuthUser("testUser", "1001", "member", "test", UserEnums.MEMBER);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);

        when(smsUtil.verifyCode("13800138000", VerificationEnums.BIND_MOBILE, "uuid-123", "123456"))
                .thenReturn(true);
        when(memberService.getById("1001")).thenReturn(null);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                memberBuyerController.bindMobile("13800138000", "123456", "uuid-123"));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_NOT_EXIST);
        verify(memberService, never()).changeMobile(any(), any());
    }

    /**
     * 目标手机号已被其他账号绑定时抛出手机号重复异常
     */
    @Test
    @DisplayName("换绑手机-手机号已被占用")
    void bindMobileThrowsWhenMobileAlreadyBound() {
        AuthUser authUser = new AuthUser("testUser", "1001", "member", "test", UserEnums.MEMBER);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);

        Member currentMember = new Member();
        currentMember.setId("1001");
        currentMember.setUsername("testUser");

        Member existingMobileMember = new Member();
        existingMobileMember.setId("2002");
        existingMobileMember.setMobile("13800138000");

        when(smsUtil.verifyCode("13800138000", VerificationEnums.BIND_MOBILE, "uuid-123", "123456"))
                .thenReturn(true);
        when(memberService.getById("1001")).thenReturn(currentMember);
        when(memberService.findByMobile("13800138000")).thenReturn(existingMobileMember);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                memberBuyerController.bindMobile("13800138000", "123456", "uuid-123"));

        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_MOBILE_REPEATABLE_ERROR);
        verify(memberService, never()).changeMobile(any(), any());
    }

    /**
     * 验证码正确且目标手机号未被绑定时换绑成功
     */
    @Test
    @DisplayName("换绑手机-成功")
    void bindMobileSuccess() {
        AuthUser authUser = new AuthUser("testUser", "1001", "member", "test", UserEnums.MEMBER);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(authUser);

        Member currentMember = new Member();
        currentMember.setId("1001");
        currentMember.setUsername("testUser");

        when(smsUtil.verifyCode("13800138000", VerificationEnums.BIND_MOBILE, "uuid-123", "123456"))
                .thenReturn(true);
        when(memberService.getById("1001")).thenReturn(currentMember);
        when(memberService.findByMobile("13800138000")).thenReturn(null);
        when(memberService.changeMobile("1001", "13800138000")).thenReturn(true);

        ResultMessage<Object> result = memberBuyerController.bindMobile("13800138000", "123456", "uuid-123");

        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getResult()).isEqualTo(true);
        verify(memberService).changeMobile("1001", "13800138000");
    }
}
