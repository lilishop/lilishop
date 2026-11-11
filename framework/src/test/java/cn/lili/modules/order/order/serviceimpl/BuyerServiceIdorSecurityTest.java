package cn.lili.modules.order.order.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.message.entity.dos.MemberMessage;
import cn.lili.modules.message.entity.enums.MessageStatusEnum;
import cn.lili.modules.message.serviceimpl.MemberMessageServiceImpl;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderComplaint;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dos.OrderPackage;
import cn.lili.modules.order.order.entity.dto.OrderComplaintDTO;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.entity.vo.OrderInfoVO;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 买家业务服务层越权防护单元测试，覆盖包裹物流、站内信及交易投诉的底层归属校验。
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
class BuyerServiceIdorSecurityTest {

    private static final String BUYER_ID = "buyer-1";
    private static final String OTHER_BUYER_ID = "buyer-2";
    private static final String ORDER_SN = "SN-1001";
    private static final String MESSAGE_ID = "msg-1001";

    private OrderPackageServiceImpl orderPackageService;
    private OrderMapper orderMapper;

    private MemberMessageServiceImpl memberMessageService;

    private OrderComplaintServiceImpl orderComplaintService;
    private OrderService orderService;
    private OrderItemService orderItemService;

    private MockedStatic<UserContext> userContextMock;

    @BeforeAll
    static void initEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, OrderPackage.class);
        TableInfoHelper.initTableInfo(assistant, Order.class);
        TableInfoHelper.initTableInfo(assistant, MemberMessage.class);
        TableInfoHelper.initTableInfo(assistant, OrderComplaint.class);
    }

    @BeforeEach
    void setUp() {
        AuthUser user = new AuthUser();
        user.setId(BUYER_ID);
        user.setUsername("testBuyer");
        user.setRole(UserEnums.MEMBER);

        userContextMock = mockStatic(UserContext.class);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(user);

        orderPackageService = spy(new OrderPackageServiceImpl());
        orderMapper = mock(OrderMapper.class);
        ReflectionTestUtils.setField(orderPackageService, "orderMapper", orderMapper);

        memberMessageService = spy(new MemberMessageServiceImpl());

        orderComplaintService = spy(new OrderComplaintServiceImpl());
        orderService = mock(OrderService.class);
        orderItemService = mock(OrderItemService.class);
        ReflectionTestUtils.setField(orderComplaintService, "orderService", orderService);
        ReflectionTestUtils.setField(orderComplaintService, "orderItemService", orderItemService);
    }

    @AfterEach
    void tearDown() {
        if (userContextMock != null) {
            userContextMock.close();
        }
    }

    @Test
    void getOrderPackageVOListOtherBuyerThrowsAuthorityError() {
        Order otherOrder = new Order();
        otherOrder.setSn(ORDER_SN);
        otherOrder.setMemberId(OTHER_BUYER_ID);
        when(orderMapper.selectOne(any(Wrapper.class))).thenReturn(otherOrder);

        ServiceException ex = assertThrows(ServiceException.class,
                () -> orderPackageService.getOrderPackageVOList(ORDER_SN));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
    }

    @Test
    void editStatusOtherMemberMessageThrowsAuthorityError() {
        MemberMessage otherMsg = new MemberMessage();
        otherMsg.setId(MESSAGE_ID);
        otherMsg.setMemberId(OTHER_BUYER_ID);
        doReturn(otherMsg).when(memberMessageService).getById(MESSAGE_ID);

        ServiceException ex = assertThrows(ServiceException.class,
                () -> memberMessageService.editStatus(MessageStatusEnum.ALREADY_READY.name(), MESSAGE_ID));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(memberMessageService, never()).updateById(any(MemberMessage.class));
    }

    @Test
    void deleteMessageOtherMemberMessageThrowsAuthorityError() {
        MemberMessage otherMsg = new MemberMessage();
        otherMsg.setId(MESSAGE_ID);
        otherMsg.setMemberId(OTHER_BUYER_ID);
        doReturn(otherMsg).when(memberMessageService).getById(MESSAGE_ID);

        ServiceException ex = assertThrows(ServiceException.class,
                () -> memberMessageService.deleteMessage(MESSAGE_ID));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(memberMessageService, never()).removeById(any(MemberMessage.class));
    }

    @Test
    void addOrderComplainOtherOrderThrowsAuthorityError() {
        Order otherOrder = new Order();
        otherOrder.setSn(ORDER_SN);
        otherOrder.setMemberId(OTHER_BUYER_ID);

        OrderDetailVO orderDetailVO = new OrderDetailVO();
        orderDetailVO.setOrder(new OrderInfoVO(otherOrder, null));
        orderDetailVO.setOrderItems(Collections.emptyList());

        when(orderService.queryDetail(ORDER_SN)).thenReturn(orderDetailVO);

        OrderComplaintDTO dto = new OrderComplaintDTO();
        dto.setOrderSn(ORDER_SN);
        dto.setSkuId("sku-1");

        ServiceException ex = assertThrows(ServiceException.class,
                () -> orderComplaintService.addOrderComplain(dto));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(orderComplaintService, never()).save(any(OrderComplaint.class));
    }
}
