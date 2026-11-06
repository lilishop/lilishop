package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.GiftCardCashActivity;
import cn.lili.modules.promotion.entity.vos.GiftCardCashActivityVO;
import cn.lili.modules.promotion.entity.vos.PickupCardSkuVO;
import cn.lili.modules.promotion.mapper.GiftCardCashActivityMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashCredentialMapper;
import cn.lili.modules.promotion.service.PickupCardRuleService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * 礼品卡活动列表和详情响应兼容性测试，验证统计及提货商品迁移到 VO 后保留原 JSON 字段和分页数据。
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
class GiftCardCashActivityDetailTest {

    private GiftCardCashServiceImpl service;
    private GiftCardCashActivityMapper activities;
    private GiftCardCashActivity activity;
    private PickupCardRuleService pickupCardRuleService;

    /**
     * 初始化模拟依赖和活动数据，不连接数据库或 Redis。
     */
    @BeforeEach
    void setUp() {
        service = new GiftCardCashServiceImpl();
        activities = mock(GiftCardCashActivityMapper.class);
        GiftCardCashCredentialMapper credentials = mock(GiftCardCashCredentialMapper.class);
        pickupCardRuleService = mock(PickupCardRuleService.class);
        ReflectionTestUtils.setField(service, "activityMapper", activities);
        ReflectionTestUtils.setField(service, "credentialMapper", credentials);
        ReflectionTestUtils.setField(service, "pickupCardRuleService", pickupCardRuleService);
        activity = new GiftCardCashActivity();
        activity.setId("activity");
        activity.setGiftCardName("活动名称");
        activity.setFaceValue(new BigDecimal("100.00"));
        activity.setStockTotal(10);
        when(activities.selectById("activity")).thenReturn(activity);
        when(credentials.selectCount(any())).thenReturn(2L);
    }

    /**
     * 提货卡详情保留基础字段、库存统计和原 pickupGoods JSON 路径。
     */
    @Test
    void pickupDetailPreservesResponseFields() {
        activity.setCardType("PICKUP");
        activity.setPickupMode("ONE");
        activity.setPickupCount(1);
        activity.setPickupSkuIds("[\"sku\"]");
        PickupCardSkuVO goods = new PickupCardSkuVO();
        goods.setId("sku");
        goods.setGoodsName("候选商品");
        when(pickupCardRuleService.goods(activity.getPickupSkuIds())).thenReturn(List.of(goods));

        GiftCardCashActivityVO detail = service.getActivity("activity");
        JsonNode json = new ObjectMapper().valueToTree(detail);

        assertThat(json.path("id").asText()).isEqualTo("activity");
        assertThat(json.path("giftCardName").asText()).isEqualTo("活动名称");
        assertThat(json.path("pickupMode").asText()).isEqualTo("ONE");
        assertThat(json.path("pickupCount").asInt()).isEqualTo(1);
        assertThat(json.path("pickupSkuIds").asText()).isEqualTo("[\"sku\"]");
        assertThat(json.path("producedQuantity").asInt()).isEqualTo(2);
        assertThat(json.path("issuedCardQuantity").asInt()).isEqualTo(2);
        assertThat(json.path("remainingStock").asInt()).isEqualTo(8);
        assertThat(json.path("pickupGoods").path(0).path("id").asText()).isEqualTo("sku");
        assertThat(detail).isNotSameAs(activity);
    }

    /**
     * 现金卡详情保持原面额，不触发提货商品查询。
     */
    @Test
    void cashDetailDoesNotLoadPickupGoods() {
        activity.setCardType("CASH");

        GiftCardCashActivityVO detail = service.getActivity("activity");

        assertThat(detail.getFaceValue()).isEqualByComparingTo("100.00");
        assertThat(detail.getPickupGoods()).isNull();
        verifyNoInteractions(pickupCardRuleService);
    }

    /**
     * 活动列表保留分页元数据与三个统计字段，且不逐条加载仅详情需要的候选商品。
     */
    @Test
    void pagePreservesMetadataAndAggregateFields() {
        activity.setCardType("PICKUP");
        Page<GiftCardCashActivity> raw = new Page<>(2, 20, 45);
        raw.setRecords(List.of(activity));
        when(activities.selectPage(any(), any())).thenReturn(raw);
        PageVO request = new PageVO();
        request.setPageNumber(2);
        request.setPageSize(20);

        IPage<GiftCardCashActivityVO> result = service.pageActivity(null, request);
        JsonNode json = new ObjectMapper().valueToTree(result);

        assertThat(result.getCurrent()).isEqualTo(2);
        assertThat(result.getSize()).isEqualTo(20);
        assertThat(result.getTotal()).isEqualTo(45);
        assertThat(json.path("records").path(0).path("id").asText()).isEqualTo("activity");
        assertThat(json.path("records").path(0).path("producedQuantity").asInt()).isEqualTo(2);
        assertThat(json.path("records").path(0).path("issuedCardQuantity").asInt()).isEqualTo(2);
        assertThat(json.path("records").path(0).path("remainingStock").asInt()).isEqualTo(8);
        verifyNoInteractions(pickupCardRuleService);
    }
}
