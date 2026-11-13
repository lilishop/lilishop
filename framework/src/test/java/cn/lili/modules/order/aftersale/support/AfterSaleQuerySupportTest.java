package cn.lili.modules.order.aftersale.support;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleApplyVO;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleNumVO;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleSearchParams;
import cn.lili.modules.order.aftersale.mapper.AfterSaleMapper;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.trade.entity.enums.AfterSaleRefundWayEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.promotion.service.PickupCardOrderService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 售后查询与统计支持类单元测试，验证各状态售后单聚合计数映射、空结果默认值及售后申请视图组装逻辑。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
class AfterSaleQuerySupportTest {

    private AfterSaleQuerySupport querySupport;
    private AfterSaleMapper afterSaleMapper;
    private AppointmentService appointmentService;
    private AfterSaleValidationSupport validationSupport;

    @BeforeEach
    void setUp() {
        querySupport = new AfterSaleQuerySupport();
        afterSaleMapper = mock(AfterSaleMapper.class);
        appointmentService = mock(AppointmentService.class);
        validationSupport = new AfterSaleValidationSupport(
                null, null, appointmentService, mock(PickupCardOrderService.class));
    }

    /**
     * 验证聚合查询返回多状态统计数及空结果全部置零。
     */
    @Test
    @DisplayName("聚合统计各状态售后单数量并支持空结果兜底")
    void shouldAggregateAfterSaleStatusCounts() {
        Map<String, Object> row = Map.of(
                "applyNum", 3L,
                "passNum", 2L,
                "refuseNum", 1L,
                "buyerReturnNum", 4L,
                "sellerConfirmNum", 0L,
                "sellerTerminationNum", 1L,
                "buyerCancelNum", 2L,
                "waitRefundNum", 5L,
                "completeNum", 10L
        );
        when(afterSaleMapper.selectMaps(any())).thenReturn(List.of(row));

        AfterSaleNumVO numVO = querySupport.getAfterSaleNumVO(afterSaleMapper, new AfterSaleSearchParams());
        assertThat(numVO.getApplyNum()).isEqualTo(3);
        assertThat(numVO.getWaitRefundNum()).isEqualTo(5);
        assertThat(numVO.getCompleteNum()).isEqualTo(10);

        when(afterSaleMapper.selectMaps(any())).thenReturn(Collections.emptyList());
        AfterSaleNumVO emptyVO = querySupport.getAfterSaleNumVO(afterSaleMapper, new AfterSaleSearchParams());
        assertThat(emptyVO.getApplyNum()).isZero();
        assertThat(emptyVO.getCompleteNum()).isZero();
    }

    /**
     * 验证构建申请页视图时正确区分线上/线下退款方式、虚拟/实物退货支持及单件可退金额。
     */
    @Test
    @DisplayName("构建售后申请页视图并校验未支付订单拒绝")
    void shouldBuildAfterSaleApplyVOAndRejectUnpaidOrder() {
        Order order = new Order();
        order.setSn("O1");
        order.setMemberId("M1");
        order.setOrderType(OrderTypeEnum.NORMAL.name());
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setOrderStatus(OrderStatusEnum.DELIVERED.name());
        order.setPaymentMethod(PaymentMethodEnum.WECHAT.name());

        OrderItem item = new OrderItem();
        item.setSn("OI1");
        item.setNum(2);
        item.setFlowPrice(180.0);
        PriceDetailDTO priceDetailDTO = new PriceDetailDTO();
        priceDetailDTO.setGiftCardPrice(20.0);
        item.setPriceDetailDTO(priceDetailDTO);
        item.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.NOT_APPLIED.name());

        AfterSaleApplyVO applyVO = querySupport.buildAfterSaleApplyVO(
                item, order, appointmentService, validationSupport);

        assertThat(applyVO.getRefundWay()).isEqualTo(AfterSaleRefundWayEnum.ORIGINAL.name());
        assertThat(applyVO.getReturnMoney()).isTrue();
        assertThat(applyVO.getReturnGoods()).isTrue();
        assertThat(applyVO.getApplyRefundPrice()).isEqualTo(100.0);

        order.setPayStatus(PayStatusEnum.UNPAID.name());
        assertThatThrownBy(() -> querySupport.buildAfterSaleApplyVO(
                item, order, appointmentService, validationSupport))
                .isInstanceOf(ServiceException.class)
                .extracting(e -> ((ServiceException) e).getResultCode())
                .isEqualTo(ResultCode.AFTER_SALES_NOT_PAY_ERROR);
    }
}
