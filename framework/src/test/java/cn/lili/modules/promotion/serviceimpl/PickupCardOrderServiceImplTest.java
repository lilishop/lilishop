package cn.lili.modules.promotion.serviceimpl;

import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderGiftCardRecord;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.entity.vo.OrderInfoVO;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.mapper.OrderGiftCardRecordMapper;
import cn.lili.modules.order.order.serviceimpl.OrderServiceImpl;
import cn.lili.modules.promotion.service.PickupCardOrderService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * 流水识别、列表批量回显及详情 JSON 兼容性测试，不依赖订单表提货字段。
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
class PickupCardOrderServiceImplTest {

    /** 关联查询只使用原兑换记录，批量输入只查询一次数据库。 */
    @Test
    void resolvesCredentialIdsInOneQuery() {
        TableInfoHelper.initTableInfo(new MapperBuilderAssistant(new MybatisConfiguration(), "pickup-relation"), OrderGiftCardRecord.class);
        OrderGiftCardRecordMapper records = mock(OrderGiftCardRecordMapper.class);
        PickupCardOrderServiceImpl service = new PickupCardOrderServiceImpl();
        ReflectionTestUtils.setField(service, "recordMapper", records);
        OrderGiftCardRecord row = new OrderGiftCardRecord();
        row.setOrderSn("order");
        row.setCredentialId("card");
        when(records.selectList(any())).thenAnswer(call -> {
            LambdaQueryWrapper<OrderGiftCardRecord> query = call.getArgument(0);
            query.getSqlSegment();
            assertThat(query.getParamNameValuePairs().values()).contains("PICKUP_REDEEM", false);
            return List.of(row);
        });

        assertThat(service.getCredentialIds(List.of("order", "normal", "order"))).containsExactlyEntriesOf(Map.of("order", "card"));
        verify(records, times(1)).selectList(any());
        assertThat(service.getCredentialIds(List.of())).isEmpty();
        assertThat(service.getCredentialId(null)).isNull();
        verifyNoMoreInteractions(records);
    }

    /** 当前页关联一次批量回填，保留普通订单和分页元数据。 */
    @Test
    void enrichesPageWithoutPerOrderQueries() {
        PickupCardOrderService relations = mock(PickupCardOrderService.class);
        OrderServiceImpl service = new OrderServiceImpl();
        ReflectionTestUtils.setField(service, "pickupCardOrderService", relations);
        OrderSimpleVO pickup = new OrderSimpleVO();
        pickup.setSn("pickup");
        OrderSimpleVO normal = new OrderSimpleVO();
        normal.setSn("normal");
        Page<OrderSimpleVO> page = new Page<>(2, 20, 45);
        page.setRecords(List.of(pickup, normal));
        when(relations.getCredentialIds(List.of("pickup", "normal"))).thenReturn(Map.of("pickup", "card"));

        Object result = ReflectionTestUtils.invokeMethod(service, "fillPickupOrderInfo", page);

        assertThat(result).isSameAs(page);
        assertThat(pickup.getPickupCardId()).isEqualTo("card");
        assertThat(normal.getPickupCardId()).isNull();
        assertThat(page.getTotal()).isEqualTo(45);
        verify(relations).getCredentialIds(List.of("pickup", "normal"));
        verifyNoMoreInteractions(relations);
    }

    /** 详情继续在 order.pickupCardId 返回展示关联，原始价格 JSON 不被快照转换改写。 */
    @Test
    void preservesDetailJsonContract() {
        Order order = new Order();
        order.setSn("order");
        order.setGoodsPrice(123.45D);
        order.setOrderType("NORMAL");
        order.setOrderStatus("PAID");
        order.setPayStatus("PAID");
        order.setDeliverStatus("UNDELIVERED");
        order.setDeliveryMethod("LOGISTICS");
        order.setPriceDetail(null);
        OrderDetailVO detail = new OrderDetailVO(new OrderInfoVO(order, "card"), List.of(), List.of(), null);

        JsonNode json = new ObjectMapper().valueToTree(detail);

        assertThat(json.path("order").path("pickupCardId").asText()).isEqualTo("card");
        assertThat(json.path("order").path("sn").asText()).isEqualTo("order");
        assertThat(json.path("order").path("goodsPrice").asDouble()).isEqualTo(123.45D);
        assertThat(detail.getOrder().getPriceDetail()).isNull();
        assertThat(new ObjectMapper().valueToTree(order).has("pickupCardId")).isFalse();
    }
}
