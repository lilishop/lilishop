package cn.lili.modules.member.util;

import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.mapper.StoreFlowMapper;
import cn.lili.modules.order.order.serviceimpl.StoreFlowServiceImpl;
import cn.lili.modules.payment.service.RefundLogService;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * 退款流水实际写入的会员补贴、结算金额及重复通知保护。
 * @author Bulbasaur
 * @since 2026-11-05
 */
class PaidMemberRefundTest {
    /**
     * 验证三件商品逐件退款时，0.01 元补贴按累计数量分摊为 0、0.01、0 元。
     * 退款结算金额为 1、1.01、1 元，不依赖会员订单来源，退完后标记全退。
     * 本例尾差落在第二笔，累计冲回总额必须等于原补贴。
     */
    @Test void partialRefundReversesOriginalSubsidyAndLastRefundTakesRoundingRemainder() {
        var service=spy(new StoreFlowServiceImpl());
        var mapper=mock(StoreFlowMapper.class);
        ReflectionTestUtils.setField(service,"baseMapper",mapper);
        ReflectionTestUtils.setField(service,"refundLogService",mock(RefundLogService.class));
        StoreFlow pay=new StoreFlow();pay.setOrderItemSn("item");pay.setNum(3);
        pay.setPaidMemberSubsidyPrice(0.01);
        pay.setCommissionPrice(0D);pay.setDistributionRebate(0D);pay.setSiteCouponCommission(0D);
        pay.setFinalPrice(3D);pay.setBillPrice(3.01);pay.setProfitSharing("{}");
        doReturn(pay).when(service).getOne(any());doReturn(0L).when(service).count(any());
        doReturn(true).when(service).save(any(StoreFlow.class));doReturn(true).when(service).updateById(any(StoreFlow.class));
        when(mapper.getRefundNum("item")).thenReturn(0,0,1,1,2,2);
        try(var snow=mockStatic(SnowFlake.class)) {
            snow.when(()->SnowFlake.createStr("SF")).thenReturn("refund-flow");
            for(int i=0;i<3;i++) {
                AfterSale after=new AfterSale();after.setSn("after-"+i);after.setOrderItemSn("item");after.setNum(1);after.setActualRefundPrice(1D);
                service.refundOrder(after);
            }
        }
        var flows=org.mockito.ArgumentCaptor.forClass(StoreFlow.class);
        verify(service,times(3)).save(flows.capture());
        assertThat(flows.getAllValues()).extracting(StoreFlow::getPaidMemberSubsidyPrice).containsExactly(0D,0.01,0D);
        assertThat(flows.getAllValues()).extracting(StoreFlow::getBillPrice).containsExactly(1D,1.01,1D);
        assertThat(pay.getFullRefund()).isTrue();
    }
    /**
     * 模拟同一售后单已存在退款流水，验证重复处理不再保存新流水。
     */
    @Test void repeatedAfterSaleCannotCreateSecondRefundFlow() {
        var service=spy(new StoreFlowServiceImpl());
        doReturn(new StoreFlow()).when(service).getOne(any());doReturn(1L).when(service).count(any());
        AfterSale after=new AfterSale();after.setOrderItemSn("item");after.setSn("same-after-sale");
        service.refundOrder(after);
        verify(service,never()).save(any(StoreFlow.class));
    }
}
