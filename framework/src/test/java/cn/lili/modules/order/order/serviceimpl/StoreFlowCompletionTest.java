package cn.lili.modules.order.order.serviceimpl;

import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.member.service.MemberOrderPointService;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.mapper.StoreFlowMapper;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;
import java.util.List;
import static org.mockito.Mockito.*;

/**
 * 交易完结入口测试，覆盖多流水去重、失败保留待处理状态及订单间失败隔离。
 * @author Bulbasaur
 * @since 2026-11-05
 */
class StoreFlowCompletionTest {
    private StoreFlowServiceImpl service;
    private StoreFlowMapper mapper;
    private MemberOrderPointService points;
    private DistributionOrderService distribution;

    /** 隔离数据库更新，验证积分、分销和出账状态推进的先后顺序。 */
    @BeforeEach void setup() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), ""), StoreFlow.class);
        service = spy(new StoreFlowServiceImpl());
        mapper = mock(StoreFlowMapper.class);
        points = mock(MemberOrderPointService.class);
        distribution = mock(DistributionOrderService.class);
        ReflectionTestUtils.setField(service, "baseMapper", mapper);
        ReflectionTestUtils.setField(service, "memberOrderPointService", points);
        ReflectionTestUtils.setField(service, "distributionOrderService", distribution);
        doReturn(true).when(service).update(any(Wrapper.class));
    }

    /** 构造订单的支付或退款流水，ID 用于限定状态更新范围。 */
    private StoreFlow flow(String id, String order, String type) {
        var flow = new StoreFlow();
        flow.setId(id); flow.setOrderSn(order); flow.setFlowType(type);
        return flow;
    }

    /** 同一订单多商品项及退款流水只发一次积分，成功后才推进该批流水。 */
    @Test void awardsOncePerOrderBeforeAdvancingFlows() {
        var pay = flow("1", "order", "PAY");
        var refund = flow("2", "order", "REFUND");
        when(mapper.completeList()).thenReturn(List.of(pay, refund));
        when(points.complete(any())).thenReturn(true);
        service.updateProfitSharingStatus();
        var sequence = inOrder(points, distribution, service);
        sequence.verify(distribution).completeOrder(pay);
        sequence.verify(distribution).completeOrder(refund);
        sequence.verify(points).complete("order");
        sequence.verify(service).update(any(Wrapper.class));
        verify(points, times(1)).complete(any());
    }

    /** 积分发放失败时不推进该订单，其他订单仍可正常处理。 */
    @Test void failedAwardRetainsFlowsAndDoesNotBlockOtherOrders() {
        var failed = flow("1", "failed", "PAY");
        var success = flow("2", "success", "PAY");
        when(mapper.completeList()).thenReturn(List.of(failed, success));
        when(points.complete(any())).thenAnswer(call -> {
            String orderSn = call.getArgument(0);
            if ("failed".equals(orderSn)) throw new IllegalStateException("积分失败");
            return true;
        });
        service.updateProfitSharingStatus();
        verify(distribution).completeOrder(failed);
        verify(distribution).completeOrder(success);
        verify(service, times(1)).update(any(Wrapper.class));
    }

    /** 积分复核发现尚未完结时保留原流水状态，已完成分销不推进出账。 */
    @Test void unfinishedOrderIsNotAdvanced() {
        when(mapper.completeList()).thenReturn(List.of(flow("1", "order", "PAY")));
        when(points.complete(any())).thenReturn(false);
        service.updateProfitSharingStatus();
        verify(distribution).completeOrder(any());
        verify(service, never()).update(any(Wrapper.class));
    }

    /** 分销失败时不赠送积分、不推进流水，其他订单不受影响。 */
    @Test void failedDistributionRetainsFlowsAndSkipsPoints() {
        var failed = flow("1", "failed", "PAY");
        var success = flow("2", "success", "PAY");
        when(mapper.completeList()).thenReturn(List.of(failed, success));
        doThrow(new IllegalStateException("分销失败")).when(distribution).completeOrder(failed);
        when(points.complete("success")).thenReturn(true);
        service.updateProfitSharingStatus();
        verify(points, never()).complete("failed");
        verify(points).complete("success");
        verify(service, times(1)).update(any(Wrapper.class));
    }

    /** 没有待完结流水时不发积分、不做空条件更新。 */
    @Test void emptyBatchDoesNothing() {
        when(mapper.completeList()).thenReturn(List.of());
        service.updateProfitSharingStatus();
        verifyNoInteractions(points, distribution);
        verify(service, never()).update(any(Wrapper.class));
    }
}
