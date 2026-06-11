package cn.lili.modules.verification.service.impl;

import cn.lili.cache.Cache;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.VerificationCodeProperties;
import cn.lili.modules.verification.entity.enums.VerificationEnums;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class VerificationServiceImplTest {

    @Test
    void checkAllowsLoginWhenLoginBypassEnabled() {
        VerificationServiceImpl service = new VerificationServiceImpl();
        Cache<?> cache = mock(Cache.class);
        VerificationCodeProperties properties = new VerificationCodeProperties();
        properties.setLoginBypassEnabled(true);
        ReflectionTestUtils.setField(service, "cache", cache);
        ReflectionTestUtils.setField(service, "verificationCodeProperties", properties);

        boolean result = service.check("codex-uat", VerificationEnums.LOGIN);

        assertThat(result).isTrue();
        verify(cache, never()).remove(VerificationServiceImpl.cacheResult(VerificationEnums.LOGIN, "codex-uat"));
    }

    @Test
    void checkRejectsLoginWhenBypassDisabledAndCacheMisses() {
        VerificationServiceImpl service = new VerificationServiceImpl();
        Cache<?> cache = mock(Cache.class);
        VerificationCodeProperties properties = new VerificationCodeProperties();
        properties.setLoginBypassEnabled(false);
        ReflectionTestUtils.setField(service, "cache", cache);
        ReflectionTestUtils.setField(service, "verificationCodeProperties", properties);
        when(cache.remove(VerificationServiceImpl.cacheResult(VerificationEnums.LOGIN, "codex-uat"))).thenReturn(false);

        ServiceException exception = assertThrows(
                ServiceException.class,
                () -> service.check("codex-uat", VerificationEnums.LOGIN)
        );

        assertThat(exception.getResultCode()).isEqualTo(ResultCode.VERIFICATION_CODE_INVALID);
    }
}
