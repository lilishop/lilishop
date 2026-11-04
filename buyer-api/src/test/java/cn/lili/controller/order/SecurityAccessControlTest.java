package cn.lili.controller.order;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.controller.wallet.MemberReceiptController;
import cn.lili.modules.member.entity.vo.MemberReceiptAddVO;
import cn.lili.modules.member.entity.vo.MemberReceiptVO;
import cn.lili.modules.member.service.MemberReceiptService;
import cn.lili.modules.order.aftersale.entity.dos.AfterSaleLog;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleVO;
import cn.lili.modules.order.aftersale.service.AfterSaleLogService;
import cn.lili.modules.order.aftersale.service.AfterSaleService;
import cn.lili.modules.order.order.entity.dos.Receipt;
import cn.lili.modules.order.order.entity.dto.ReceiptSearchParams;
import cn.lili.modules.order.order.entity.vo.OrderComplaintVO;
import cn.lili.modules.order.order.service.OrderComplaintCommunicationService;
import cn.lili.modules.order.order.service.OrderComplaintService;
import cn.lili.modules.order.order.service.ReceiptService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class SecurityAccessControlTest {

    @Mock
    private ReceiptService receiptService;

    @Mock
    private OrderComplaintService orderComplaintService;

    @Mock
    private OrderComplaintCommunicationService orderComplaintCommunicationService;

    @Mock
    private AfterSaleService afterSaleService;

    @Mock
    private AfterSaleLogService afterSaleLogService;

    @Mock
    private MemberReceiptService memberReceiptService;

    @InjectMocks
    private ReceiptBuyerController receiptBuyerController;

    @InjectMocks
    private OrderComplaintBuyerController orderComplaintBuyerController;

    @InjectMocks
    private AfterSaleBuyerController afterSaleBuyerController;

    @InjectMocks
    private MemberReceiptController memberReceiptController;

    private MockedStatic<UserContext> userContextMock;
    private AuthUser currentUser;

    /**
     * 初始化当前用例的模拟依赖及测试数据。
     */
    @BeforeEach
    void setUp() {
        currentUser = new AuthUser();
        currentUser.setId("buyer-1001");
        currentUser.setUsername("buyerUser");
        currentUser.setNickName("BuyerNick");
        currentUser.setRole(UserEnums.MEMBER);

        userContextMock = mockStatic(UserContext.class);
        userContextMock.when(UserContext::getCurrentUser).thenReturn(currentUser);
    }

    /**
     * 释放当前用例创建的静态模拟资源。
     */
    @AfterEach
    void tearDown() {
        if (userContextMock != null) {
            userContextMock.close();
        }
    }

    // ==================== 1. ReceiptBuyerController 越权防御测试 ====================

    /**
     * 验证售后单所属会员可以查询详情。
     */
    @Test
    @DisplayName("发票详情：用户查询归属于本人的发票应正常放行")
    void getDetailOwnerAllowed() {
        Receipt receipt = new Receipt();
        receipt.setId("receipt-1");
        receipt.setMemberId("buyer-1001");
        when(receiptService.getDetail("receipt-1")).thenReturn(receipt);

        ResultMessage<Receipt> result = receiptBuyerController.getDetail("receipt-1");
        assertThat(result.getResult()).isNotNull();
        assertThat(result.getResult().getId()).isEqualTo("receipt-1");
    }

    /**
     * 验证其他会员不能查询售后单详情。
     */
    @Test
    @DisplayName("发票详情：用户越权查询归属于他人的发票应拦截并抛出 USER_AUTHORITY_ERROR")
    void getDetailNonOwnerForbidden() {
        Receipt receipt = new Receipt();
        receipt.setId("receipt-other");
        receipt.setMemberId("buyer-9999");
        when(receiptService.getDetail("receipt-other")).thenReturn(receipt);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                receiptBuyerController.getDetail("receipt-other")
        );
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
    }

    /**
     * 验证查询不存在的售后单时抛出异常。
     */
    @Test
    @DisplayName("发票详情：查询不存在的发票应抛出 USER_RECEIPT_NOT_EXIST")
    void getDetailNotExistThrowsException() {
        when(receiptService.getDetail("receipt-404")).thenReturn(null);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                receiptBuyerController.getDetail("receipt-404")
        );
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_RECEIPT_NOT_EXIST);
    }

    /**
     * 验证售后分页查询强制使用当前会员ID。
     */
    @Test
    @DisplayName("发票分页：强制绑定当前登录用户 memberId，防止全量发票数据泄露或跨用户越权")
    void getPageForcesCurrentMemberId() {
        ReceiptSearchParams params = new ReceiptSearchParams();
        params.setMemberId("arbitrary-or-null");

        receiptBuyerController.getPage(params, new PageVO());

        assertThat(params.getMemberId()).isEqualTo("buyer-1001");
        verify(receiptService).getReceiptData(eq(params), any());
    }

    /**
     * 验证保存售后申请时强制使用当前会员ID。
     */
    @Test
    @DisplayName("发票保存：强制覆盖 memberId 为当前登录用户，防止伪造其他用户发票")
    void saveForcesCurrentMemberId() {
        Receipt receipt = new Receipt();
        receipt.setMemberId("attacker-member-id");

        receiptBuyerController.save(receipt);

        assertThat(receipt.getMemberId()).isEqualTo("buyer-1001");
        assertThat(receipt.getMemberName()).isEqualTo("buyerUser");
        verify(receiptService).saveReceipt(receipt);
    }

    // ==================== 2. OrderComplaintBuyerController 越权防御测试 ====================

    /**
     * 验证售后单所属会员可以添加沟通记录。
     */
    @Test
    @DisplayName("交易投诉对话：买家向属于本人的投诉添加沟通记录应正常放行")
    void addCommunicationOwnerAllowed() {
        OrderComplaintVO complaintVO = new OrderComplaintVO();
        complaintVO.setMemberId("buyer-1001");
        when(orderComplaintService.getOrderComplainById("complain-1")).thenReturn(complaintVO);

        orderComplaintBuyerController.addCommunication("complain-1", "商家何时发货？");

        verify(orderComplaintCommunicationService).addCommunication(any());
    }

    /**
     * 验证其他会员不能添加售后沟通记录。
     */
    @Test
    @DisplayName("交易投诉对话：买家向他人的投诉注入沟通记录应拦截并抛出 USER_AUTHORITY_ERROR")
    void addCommunicationNonOwnerForbidden() {
        OrderComplaintVO complaintVO = new OrderComplaintVO();
        complaintVO.setMemberId("buyer-9999");
        when(orderComplaintService.getOrderComplainById("complain-other")).thenReturn(complaintVO);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                orderComplaintBuyerController.addCommunication("complain-other", "注入恶意沟通内容")
        );
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(orderComplaintCommunicationService, never()).addCommunication(any());
    }

    // ==================== 3. AfterSaleBuyerController 越权防御测试 ====================

    /**
     * 验证售后单所属会员可以查询售后日志。
     */
    @Test
    @DisplayName("售后日志：查看属于本人的售后单日志应正常放行")
    void getAfterSaleLogOwnerAllowed() {
        AfterSaleVO afterSale = new AfterSaleVO();
        afterSale.setMemberId("buyer-1001");
        when(afterSaleService.getAfterSale("AS-1001")).thenReturn(afterSale);
        when(afterSaleLogService.getAfterSaleLog("AS-1001")).thenReturn(Collections.singletonList(new AfterSaleLog()));

        ResultMessage<List<AfterSaleLog>> result = afterSaleBuyerController.getAfterSaleLog("AS-1001");
        assertThat(result.getResult()).hasSize(1);
        verify(afterSaleLogService).getAfterSaleLog("AS-1001");
    }

    /**
     * 验证其他会员不能查询售后日志。
     */
    @Test
    @DisplayName("售后日志：查看他人的售后单日志应拦截并抛出 USER_AUTHORITY_ERROR")
    void getAfterSaleLogNonOwnerForbidden() {
        AfterSaleVO afterSale = new AfterSaleVO();
        afterSale.setMemberId("buyer-9999");
        when(afterSaleService.getAfterSale("AS-OTHER")).thenReturn(afterSale);

        ServiceException ex = assertThrows(ServiceException.class, () ->
                afterSaleBuyerController.getAfterSaleLog("AS-OTHER")
        );
        assertThat(ex.getResultCode()).isEqualTo(ResultCode.USER_AUTHORITY_ERROR);
        verify(afterSaleLogService, never()).getAfterSaleLog(any());
    }

    // ==================== 4. MemberReceiptController 越权防御测试 ====================

    /**
     * 验证发票分页查询限定当前会员。
     */
    @Test
    @DisplayName("客户发票分页：强制绑定当前用户 memberId")
    void memberReceiptPageForcesCurrentMemberId() {
        MemberReceiptVO vo = new MemberReceiptVO();
        memberReceiptController.page(vo, new PageVO());

        assertThat(vo.getMemberId()).isEqualTo("buyer-1001");
        verify(memberReceiptService).getPage(eq(vo), any());
    }

    /**
     * 验证修改发票时向服务传递当前会员ID。
     */
    @Test
    @DisplayName("客户发票修改：正确传入当前用户 memberId")
    void memberReceiptUpdatePassesCurrentUserMemberId() {
        MemberReceiptAddVO addVO = new MemberReceiptAddVO();
        memberReceiptController.update("receipt-id-1", addVO);

        assertThat(addVO.getId()).isEqualTo("receipt-id-1");
        verify(memberReceiptService).editMemberReceipt(eq(addVO), eq("buyer-1001"));
    }
}
