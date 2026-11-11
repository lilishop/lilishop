package cn.lili.controller.order;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.controller.message.MemberMessageBuyerController;
import cn.lili.controller.wallet.MemberWithdrawApplyBuyerController;
import cn.lili.modules.message.entity.dos.MemberMessage;
import cn.lili.modules.message.entity.enums.MessageStatusEnum;
import cn.lili.modules.message.service.MemberMessageService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderComplaint;
import cn.lili.modules.order.order.entity.dto.OrderComplaintDTO;
import cn.lili.modules.order.order.service.OrderComplaintService;
import cn.lili.modules.order.order.service.OrderPackageService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.wallet.entity.dos.MemberWithdrawApply;
import cn.lili.modules.wallet.entity.vo.WechatTransferInfoVO;
import cn.lili.modules.wallet.service.MemberWithdrawApplyService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 买家端越权防护单元测试，覆盖订单物流、站内信、交易投诉及提现转账信息的归属隔离。
 *
 * @author Bulbasaur
 * @since 2026-11-11
 */
@ExtendWith(MockitoExtension.class)
class BuyerIdorSecurityTest {

    private static final String BUYER_ID = "buyer-1";
    private static final String OTHER_BUYER_ID = "buyer-2";
    private static final String ORDER_SN = "SN-1001";
    private static final String MESSAGE_ID = "msg-1001";
    private static final String WITHDRAW_APPLY_ID = "apply-1001";

    @Mock
    private OrderService orderService;

    @Mock
    private OrderPackageService orderPackageService;

    @Mock
    private MemberMessageService memberMessageService;

    @Mock
    private OrderComplaintService orderComplaintService;

    @Mock
    private MemberWithdrawApplyService memberWithdrawApplyService;

    @InjectMocks
    private OrderBuyerController orderBuyerController;

    @InjectMocks
    private MemberMessageBuyerController memberMessageBuyerController;

    @InjectMocks
    private OrderComplaintBuyerController orderComplaintBuyerController;

    @InjectMocks
    private MemberWithdrawApplyBuyerController memberWithdrawApplyBuyerController;

    private MockedStatic<UserContext> userContextMock;

    @BeforeEach
    void setUp() {
        AuthUser user = new AuthUser();
        user.setId(BUYER_ID);
        user.setUsername("testBuyer");
        user.setRole(UserEnums.MEMBER);

        userContextMock = mockStatic(UserContext.class);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(user);
    }

    @AfterEach
    void tearDown() {
        if (userContextMock != null) {
            userContextMock.close();
        }
    }

    @Test
    void getTracesListOtherOrderThrowsAuthorityError() {
        Order otherOrder = new Order();
        otherOrder.setSn(ORDER_SN);
        otherOrder.setMemberId(OTHER_BUYER_ID);
        when(orderService.getBySn(ORDER_SN)).thenReturn(otherOrder);

        ServiceException ex = assertThrows(ServiceException.class,
                () -> orderBuyerController.getTracesList(ORDER_SN));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(orderPackageService, never()).getOrderPackageVOList(any());
    }

    @Test
    void getTracesListOwnOrderSuccess() {
        Order ownOrder = new Order();
        ownOrder.setSn(ORDER_SN);
        ownOrder.setMemberId(BUYER_ID);
        when(orderService.getBySn(ORDER_SN)).thenReturn(ownOrder);
        when(orderPackageService.getOrderPackageVOList(ORDER_SN)).thenReturn(Collections.emptyList());

        orderBuyerController.getTracesList(ORDER_SN);

        verify(orderPackageService).getOrderPackageVOList(ORDER_SN);
    }

    @Test
    void getPackageOtherOrderThrowsAuthorityError() {
        Order otherOrder = new Order();
        otherOrder.setSn(ORDER_SN);
        otherOrder.setMemberId(OTHER_BUYER_ID);
        when(orderService.getBySn(ORDER_SN)).thenReturn(otherOrder);

        ServiceException ex = assertThrows(ServiceException.class,
                () -> orderBuyerController.getPackage(ORDER_SN));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(orderPackageService, never()).getOrderPackageVOList(any());
    }

    @Test
    void getPackageOwnOrderSuccess() {
        Order ownOrder = new Order();
        ownOrder.setSn(ORDER_SN);
        ownOrder.setMemberId(BUYER_ID);
        when(orderService.getBySn(ORDER_SN)).thenReturn(ownOrder);
        when(orderPackageService.getOrderPackageVOList(ORDER_SN)).thenReturn(Collections.emptyList());

        orderBuyerController.getPackage(ORDER_SN);

        verify(orderPackageService).getOrderPackageVOList(ORDER_SN);
    }

    @Test
    void readOtherMemberMessageThrowsAuthorityError() {
        MemberMessage otherMsg = new MemberMessage();
        otherMsg.setId(MESSAGE_ID);
        otherMsg.setMemberId(OTHER_BUYER_ID);
        when(memberMessageService.getById(MESSAGE_ID)).thenReturn(otherMsg);

        ServiceException ex = assertThrows(ServiceException.class,
                () -> memberMessageBuyerController.read(MESSAGE_ID));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(memberMessageService, never()).editStatus(any(), any());
    }

    @Test
    void deleteOtherMemberMessageThrowsAuthorityError() {
        MemberMessage otherMsg = new MemberMessage();
        otherMsg.setId(MESSAGE_ID);
        otherMsg.setMemberId(OTHER_BUYER_ID);
        when(memberMessageService.getById(MESSAGE_ID)).thenReturn(otherMsg);

        ServiceException ex = assertThrows(ServiceException.class,
                () -> memberMessageBuyerController.deleteMessage(MESSAGE_ID));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(memberMessageService, never()).editStatus(any(), any());
    }

    @Test
    void readOwnMemberMessageSuccess() {
        MemberMessage ownMsg = new MemberMessage();
        ownMsg.setId(MESSAGE_ID);
        ownMsg.setMemberId(BUYER_ID);
        when(memberMessageService.getById(MESSAGE_ID)).thenReturn(ownMsg);
        when(memberMessageService.editStatus(MessageStatusEnum.ALREADY_READY.name(), MESSAGE_ID)).thenReturn(true);

        memberMessageBuyerController.read(MESSAGE_ID);
        verify(memberMessageService).editStatus(MessageStatusEnum.ALREADY_READY.name(), MESSAGE_ID);
    }

    @Test
    void addComplaintOtherOrderThrowsAuthorityError() {
        Order otherOrder = new Order();
        otherOrder.setSn(ORDER_SN);
        otherOrder.setMemberId(OTHER_BUYER_ID);
        when(orderService.getBySn(ORDER_SN)).thenReturn(otherOrder);

        OrderComplaintDTO dto = new OrderComplaintDTO();
        dto.setOrderSn(ORDER_SN);
        dto.setSkuId("sku-1");

        ServiceException ex = assertThrows(ServiceException.class,
                () -> orderComplaintBuyerController.add(dto));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(orderComplaintService, never()).addOrderComplain(any());
    }

    @Test
    void addComplaintOwnOrderSuccess() {
        Order ownOrder = new Order();
        ownOrder.setSn(ORDER_SN);
        ownOrder.setMemberId(BUYER_ID);
        when(orderService.getBySn(ORDER_SN)).thenReturn(ownOrder);
        when(orderComplaintService.addOrderComplain(any())).thenReturn(new OrderComplaint());

        OrderComplaintDTO dto = new OrderComplaintDTO();
        dto.setOrderSn(ORDER_SN);
        dto.setSkuId("sku-1");

        orderComplaintBuyerController.add(dto);
        verify(orderComplaintService).addOrderComplain(dto);
    }

    @Test
    void getWechatTransferInfoOtherMemberThrowsAuthorityError() {
        MemberWithdrawApply otherApply = new MemberWithdrawApply();
        otherApply.setId(WITHDRAW_APPLY_ID);
        otherApply.setMemberId(OTHER_BUYER_ID);
        when(memberWithdrawApplyService.getById(WITHDRAW_APPLY_ID)).thenReturn(otherApply);

        ServiceException ex = assertThrows(ServiceException.class,
                () -> memberWithdrawApplyBuyerController.getWechatTransferInfo(WITHDRAW_APPLY_ID));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(memberWithdrawApplyService, never()).getWechatTransferInfo(WITHDRAW_APPLY_ID);
    }

    @Test
    void getWechatTransferInfoOwnMemberSuccess() {
        MemberWithdrawApply ownApply = new MemberWithdrawApply();
        ownApply.setId(WITHDRAW_APPLY_ID);
        ownApply.setMemberId(BUYER_ID);
        when(memberWithdrawApplyService.getById(WITHDRAW_APPLY_ID)).thenReturn(ownApply);
        when(memberWithdrawApplyService.getWechatTransferInfo(WITHDRAW_APPLY_ID)).thenReturn(new WechatTransferInfoVO());

        memberWithdrawApplyBuyerController.getWechatTransferInfo(WITHDRAW_APPLY_ID);
        verify(memberWithdrawApplyService).getWechatTransferInfo(WITHDRAW_APPLY_ID);
    }

    @Test
    void getWechatTransferInfoNotExistThrowsWalletApplyError() {
        when(memberWithdrawApplyService.getById(WITHDRAW_APPLY_ID)).thenReturn(null);

        ServiceException ex = assertThrows(ServiceException.class,
                () -> memberWithdrawApplyBuyerController.getWechatTransferInfo(WITHDRAW_APPLY_ID));
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.WALLET_APPLY_ERROR);
        verify(memberWithdrawApplyService, never()).getWechatTransferInfo(any());
    }
}
