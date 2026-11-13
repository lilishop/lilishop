package cn.lili.modules.member.support;

import cn.lili.cache.Cache;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.support.LoginFailLimitSupport;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 客户安全与密码支持类单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
class MemberSecuritySupportTest {

    private Cache cache;
    private LoginFailLimitSupport loginFailLimitSupport;
    private ConnectService connectService;
    private MemberService memberService;
    private MemberSecuritySupport securitySupport;
    private MockedStatic<UserContext> userContextMockedStatic;

    @BeforeEach
    void setUp() {
        cache = mock(Cache.class);
        loginFailLimitSupport = mock(LoginFailLimitSupport.class);
        connectService = mock(ConnectService.class);
        memberService = mock(MemberService.class);
        securitySupport = new MemberSecuritySupport(cache, loginFailLimitSupport, connectService);
        userContextMockedStatic = Mockito.mockStatic(UserContext.class);
    }

    @AfterEach
    void tearDown() {
        userContextMockedStatic.close();
    }

    @Test
    @DisplayName("测试校验是否可初始化默认密码")
    void testCanInitPass() {
        AuthUser authUser = new AuthUser("user1", "1001", "nickname", "face", UserEnums.MEMBER);
        userContextMockedStatic.when(UserContext::getCurrentUser).thenReturn(authUser);

        Member defaultPassMember = new Member();
        defaultPassMember.setId("1001");
        defaultPassMember.setPassword(MemberService.DEFAULT_PASSWORD);
        when(memberService.getById("1001")).thenReturn(defaultPassMember);

        assertTrue(securitySupport.canInitPass(memberService));

        defaultPassMember.setPassword("custom_password_hash");
        assertFalse(securitySupport.canInitPass(memberService));
    }

    @Test
    @DisplayName("测试注销账号混淆信息")
    void testConfusionMember() {
        Member member = new Member();
        member.setId("1001");
        member.setUsername("alice");
        member.setMobile("13800000001");
        member.setNickName("爱丽丝");
        member.setDisabled(true);

        securitySupport.confusionMember(memberService, member);

        assertEquals("用户已注销", member.getNickName());
        assertFalse(member.getDisabled());
        assertTrue(member.getMobile().contains("13800000001"));
        verify(memberService).updateById(member);
    }
}
