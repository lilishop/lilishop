package cn.lili.modules.member.support;

import cn.lili.cache.Cache;
import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.member.entity.dos.Member;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * 联合登录支持类单元测试
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
class MemberConnectSupportTest {

    private ConnectService connectService;
    private Cache cache;
    private MemberConnectSupport connectSupport;

    @BeforeEach
    void setUp() {
        connectService = mock(ConnectService.class);
        cache = mock(Cache.class);
        connectSupport = new MemberConnectSupport(connectService, cache);
    }

    @Test
    @DisplayName("测试微信默认昵称带后缀自动注册")
    void testAutoRegisterWithDefaultWechatNickname() {
        ConnectAuthUser authUser = new ConnectAuthUser();
        authUser.setUsername("wx_openid_123456");
        authUser.setNickname("微信用户");
        authUser.setUuid("openid_12345678");

        AtomicBoolean handlerCalled = new AtomicBoolean(false);
        Member member = connectSupport.autoRegister(authUser, m -> handlerCalled.set(true));

        assertNotNull(member);
        assertTrue(handlerCalled.get());
        assertEquals("微信用户5678", member.getNickName());
    }

    @Test
    @DisplayName("测试微信OpenId后缀提取")
    void testOpenIdSuffix() {
        assertEquals("5678", connectSupport.openIdSuffix("12345678"));
        assertEquals("123", connectSupport.openIdSuffix("123"));
        assertNotNull(connectSupport.openIdSuffix(null));
    }
}
