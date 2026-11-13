package cn.lili.modules.member.support;

import cn.lili.cache.Cache;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.token.Token;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.enums.QRCodeLoginSessionStatusEnum;
import cn.lili.modules.member.entity.vo.QRCodeLoginSessionVo;
import cn.lili.modules.member.entity.vo.QRLoginResultVo;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.member.token.MemberTokenGenerate;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 二维码扫码登录支持类单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
class MemberQrCodeLoginSupportTest {

    private Cache cache;
    private MemberTokenGenerate memberTokenGenerate;
    private MemberQrCodeLoginSupport qrCodeLoginSupport;
    private MockedStatic<UserContext> userContextMockedStatic;

    @BeforeEach
    void setUp() {
        cache = mock(Cache.class);
        memberTokenGenerate = mock(MemberTokenGenerate.class);
        qrCodeLoginSupport = new MemberQrCodeLoginSupport(cache, memberTokenGenerate);
        userContextMockedStatic = Mockito.mockStatic(UserContext.class);
    }

    @AfterEach
    void tearDown() {
        userContextMockedStatic.close();
    }

    @Test
    @DisplayName("测试创建PC端扫码会话")
    void testCreatePcSession() {
        QRCodeLoginSessionVo session = qrCodeLoginSupport.createPcSession();

        assertNotNull(session);
        assertEquals(QRCodeLoginSessionStatusEnum.WAIT_SCANNING.getCode(), session.getStatus());
        assertNotNull(session.getToken());
        verify(cache).put(eq(session.getToken()), eq(session), anyLong(), any());
    }

    @Test
    @DisplayName("测试移动端扫码与确认")
    void testAppScannerAndConfirm() {
        AuthUser authUser = new AuthUser("user1", "1001", "nickname", "face", cn.lili.common.security.enums.UserEnums.MEMBER);
        userContextMockedStatic.when(UserContext::getCurrentUser).thenReturn(authUser);

        String token = "TOKEN_123";
        QRCodeLoginSessionVo session = new QRCodeLoginSessionVo();
        session.setDuration(20000L);
        session.setStatus(QRCodeLoginSessionStatusEnum.WAIT_SCANNING.getCode());
        when(cache.get(token)).thenReturn(session);

        Object scannerResult = qrCodeLoginSupport.appScanner(token);
        assertEquals(QRCodeLoginSessionStatusEnum.SCANNING.getCode(), scannerResult);

        boolean confirmResult = qrCodeLoginSupport.appSConfirm(token, 1);
        assertTrue(confirmResult);
        assertEquals(QRCodeLoginSessionStatusEnum.VERIFIED.getCode(), session.getStatus());
        assertEquals(1001L, session.getUserId());
    }

    @Test
    @DisplayName("测试PC端根据会话完成登录")
    void testLoginWithSession() {
        String token = "TOKEN_123";
        QRCodeLoginSessionVo session = new QRCodeLoginSessionVo();
        session.setStatus(QRCodeLoginSessionStatusEnum.VERIFIED.getCode());
        session.setUserId(1001L);
        when(cache.get(token)).thenReturn(session);

        Member member = new Member();
        member.setId("1001");
        member.setUsername("user1");

        MemberService memberService = mock(MemberService.class);
        when(memberService.getById(1001L)).thenReturn(member);

        Token mockToken = new Token();
        mockToken.setAccessToken("ACCESS_TOKEN_123");
        when(memberTokenGenerate.createToken(member, false)).thenReturn(mockToken);

        QRLoginResultVo result = qrCodeLoginSupport.loginWithSession(memberService, token);

        assertNotNull(result);
        assertEquals(QRCodeLoginSessionStatusEnum.VERIFIED.getCode(), result.getStatus());
        assertEquals(mockToken, result.getToken());
        verify(cache).vagueDel(token);
    }
}
