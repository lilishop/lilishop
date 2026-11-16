package cn.lili.buyer.test.promotion;

import cn.lili.controller.promotion.LotteryBuyerController;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.service.LotteryService;
import org.junit.jupiter.api.*;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.test.util.ReflectionTestUtils;
import java.util.List;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * 抽奖入口身份隔离单元测试，验证匿名与后台账号拒绝、会员身份仅取安全上下文。
 * 不代表HTTP过滤器、Redis会话或数据库集成验收。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
class LotteryBuyerControllerTest {
    private final LotteryService service = mock(LotteryService.class);
    private final LotteryBuyerController controller = new LotteryBuyerController();

    /**
     * 为字段注入的控制器设置模拟服务，隔离Spring容器以验证会员身份边界。
     */
    @BeforeEach void injectService() {
        ReflectionTestUtils.setField(controller, "service", service);
    }

    /**
     * 每项测试后清理线程身份，避免污染后续测试。
     */
    @AfterEach void clear() { SecurityContextHolder.clearContext(); }

    /**
     * 匿名不得抽奖、查询次数或流水。
     */
    @Test void anonymousCannotDrawOrReadMemberData() {
        assertThatThrownBy(() -> controller.draw("activity", "request")).isInstanceOf(ServiceException.class);
        assertThatThrownBy(() -> controller.chances("activity")).isInstanceOf(ServiceException.class);
        assertThatThrownBy(() -> controller.records(new PageVO(), null)).isInstanceOf(ServiceException.class);
        verifyNoInteractions(service);
    }

    /**
     * 后台身份不可用于买家抽奖。
     */
    @Test void managerCannotParticipateAsMember() {
        authenticate(UserEnums.MANAGER, "manager");
        assertThatThrownBy(() -> controller.draw("activity", "request")).isInstanceOf(ServiceException.class);
        verifyNoInteractions(service);
    }

    /**
     * 抽奖身份来自安全上下文，不接受客户端会员ID。
     */
    @Test void drawUsesAuthenticatedMember() {
        authenticate(UserEnums.MEMBER, "member-a");
        controller.draw("activity", "request");
        verify(service).draw("activity", "member-a", "request");
    }

    /**
     * 本人流水及次数查询沿用相同身份。
     */
    @Test void historyAndChancesUseAuthenticatedMember() {
        authenticate(UserEnums.MEMBER, "member-b");
        PageVO page = new PageVO(); controller.records(page, "activity"); controller.chances("activity");
        verify(service).records(page, "member-b", "activity"); verify(service).chances("activity", "member-b");
    }

    /**
     * 构造已通过过滤器的安全上下文。
     *
     * @param role 账号角色
     * @param id 账号ID
     */
    private void authenticate(UserEnums role, String id) {
        AuthUser user = mock(AuthUser.class); when(user.getRole()).thenReturn(role); when(user.getId()).thenReturn(id);
        var authentication = new UsernamePasswordAuthenticationToken(id, "", List.of()); authentication.setDetails(user);
        SecurityContextHolder.getContext().setAuthentication(authentication);
    }
}
