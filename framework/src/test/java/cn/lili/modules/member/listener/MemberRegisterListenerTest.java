package cn.lili.modules.member.listener;

import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.event.MemberRegisterEvent;
import cn.lili.modules.member.support.MemberShareSourceSupport;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

import static org.assertj.core.api.Assertions.assertThatCode;
import static org.mockito.Mockito.*;

/**
 * 客户注册事件监听器单元测试，验证事件解耦后分享来源绑定、空参防御及异常隔离行为。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
class MemberRegisterListenerTest {

    private MemberShareSourceSupport memberShareSourceSupport;
    private MemberRegisterListener listener;

    /**
     * 注入邀请来源组件的模拟对象，按当前必需依赖构造监听器。
     */
    @BeforeEach
    void setUp() {
        memberShareSourceSupport = mock(MemberShareSourceSupport.class);
        listener = new MemberRegisterListener(memberShareSourceSupport);
    }

    /**
     * 验证空事件或客户ID为空时不调用绑定服务。
     */
    @Test
    @DisplayName("空事件或缺少客户ID时直接返回，不触发绑定")
    void shouldIgnoreNullOrBlankMemberId() {
        listener.onMemberRegister(null);
        listener.onMemberRegister(new MemberRegisterEvent(this, null, "SHARE123"));

        Member memberWithoutId = new Member();
        listener.onMemberRegister(new MemberRegisterEvent(this, memberWithoutId, "SHARE123"));

        verifyNoInteractions(memberShareSourceSupport);
    }

    /**
     * 验证未填写分享邀请码时不调用绑定服务。
     */
    @Test
    @DisplayName("无分享码时直接返回，不触发绑定")
    void shouldIgnoreBlankShareCode() {
        Member member = new Member();
        member.setId("M1001");

        listener.onMemberRegister(new MemberRegisterEvent(this, member, null));
        listener.onMemberRegister(new MemberRegisterEvent(this, member, "   "));

        verifyNoInteractions(memberShareSourceSupport);
    }

    /**
     * 验证有效客户ID与分享码时正常调用绑定服务。
     */
    @Test
    @DisplayName("存在有效客户ID与分享码时正常调用绑定服务")
    void shouldInvokeBindRegisterSourceWhenValid() {
        Member member = new Member();
        member.setId("M1001");

        listener.onMemberRegister(new MemberRegisterEvent(this, member, "SHARE888"));

        verify(memberShareSourceSupport, times(1)).bindRegisterSource("M1001", "SHARE888");
    }

    /**
     * 验证当下游绑定抛出异常时被捕获隔离，不向外抛出阻断主流程。
     */
    @Test
    @DisplayName("下游绑定异常被安全捕获隔离，不影响主流程")
    void shouldSwallowAndLogExceptionWhenServiceFails() {
        Member member = new Member();
        member.setId("M1001");
        doThrow(new RuntimeException("Redis connection failed"))
                .when(memberShareSourceSupport).bindRegisterSource("M1001", "SHARE888");

        assertThatCode(() -> listener.onMemberRegister(new MemberRegisterEvent(this, member, "SHARE888")))
                .doesNotThrowAnyException();
    }

    /**
     * 验证在真实Spring上下文中通过事件总线发布事件，监听器能正确响应。
     */
    @Test
    @DisplayName("Spring应用上下文中发布注册事件可被监听器正确消费")
    void shouldReceiveEventThroughSpringContext() {
        MemberShareSourceSupport mockService = mock(MemberShareSourceSupport.class);

        try (AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext()) {
            context.registerBean(MemberShareSourceSupport.class, () -> mockService);
            context.register(MemberRegisterListener.class);
            context.refresh();

            Member member = new Member();
            member.setId("M2001");
            context.publishEvent(new MemberRegisterEvent(this, member, "INVITE666"));

            verify(mockService, times(1)).bindRegisterSource("M2001", "INVITE666");
        }
    }
}
