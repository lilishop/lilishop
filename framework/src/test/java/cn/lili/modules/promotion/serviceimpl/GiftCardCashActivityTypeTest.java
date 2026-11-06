package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.exception.ServiceException;
import cn.lili.modules.promotion.entity.dos.GiftCardCashActivity;
import cn.lili.modules.promotion.entity.dto.GiftCardCashActivityDTO;
import cn.lili.modules.promotion.mapper.GiftCardCashActivityMapper;
import cn.lili.modules.promotion.mapper.GiftCardCashCredentialMapper;
import cn.lili.modules.promotion.service.PickupCardRuleService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * 礼品卡活动类型不可变及普通更新不清空提货规则的回归测试，不连接数据库。
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
class GiftCardCashActivityTypeTest {

    private GiftCardCashServiceImpl service;
    private GiftCardCashActivityMapper activities;
    private GiftCardCashCredentialMapper credentials;
    private GiftCardCashActivity activity;
    private GiftCardCashActivityDTO request;

    /**
     * 初始化未产卡活动及服务依赖，参数中的类型由各用例指定。
     */
    @BeforeEach
    void setUp() {
        service = new GiftCardCashServiceImpl();
        activities = mock(GiftCardCashActivityMapper.class);
        credentials = mock(GiftCardCashCredentialMapper.class);
        ReflectionTestUtils.setField(service, "activityMapper", activities);
        ReflectionTestUtils.setField(service, "credentialMapper", credentials);
        ReflectionTestUtils.setField(service, "pickupCardRuleService", mock(PickupCardRuleService.class));
        activity = new GiftCardCashActivity();
        activity.setId("activity");
        activity.setGiftCardName("原活动");
        activity.setValidityType("LONG_TERM");
        activity.setStockTotal(10);
        request = new GiftCardCashActivityDTO();
        request.setGiftCardName("新活动");
        request.setValidityType("LONG_TERM");
        request.setStockTotal(10);
        when(activities.selectOne(any())).thenReturn(activity);
        when(credentials.selectCount(any())).thenReturn(0L);
    }

    /**
     * 现金卡和提货卡均禁止互相切换，未产卡时同样拒绝。
     */
    @ParameterizedTest
    @CsvSource({"CASH,PICKUP,0", "PICKUP,CASH,0", "CASH,PICKUP,3", "PICKUP,CASH,3"})
    void rejectsTypeChangeRegardlessOfProducedCount(String original, String target, long produced) {
        activity.setCardType(original);
        request.setCardType(target);
        when(credentials.selectCount(any())).thenReturn(produced);

        assertThatThrownBy(() -> service.updateActivity("activity", request))
                .isInstanceOf(ServiceException.class).hasMessageContaining("不允许切换");

        verify(activities, never()).updateById(any(GiftCardCashActivity.class));
    }

    /**
     * 兼容旧编辑请求省略类型，必须保留已保存的卡类型和有效提货规则。
     */
    @ParameterizedTest
    @ValueSource(strings = {"CASH", "PICKUP"})
    void omittedTypeKeepsExistingType(String type) {
        activity.setCardType(type);
        activity.setFaceValue("PICKUP".equals(type) ? BigDecimal.ZERO : BigDecimal.TEN);
        request.setFaceValue(activity.getFaceValue());
        if ("PICKUP".equals(type)) {
            activity.setPickupMode("ONE");
            activity.setPickupCount(1);
            activity.setPickupSkuIds("[\"sku\"]");
            request.setPickupMode("ONE");
            request.setPickupCount(1);
            request.setPickupSkuIds(List.of("sku"));
        }

        service.updateActivity("activity", request);

        assertThat(activity.getCardType()).isEqualTo(type);
        assertThat(request.getCardType()).isEqualTo(type);
        if ("PICKUP".equals(type)) assertThat(activity.getPickupSkuIds()).isEqualTo("[\"sku\"]");
        verify(activities).updateById(activity);
    }

    /**
     * 使用实际 MyBatis 映射生成更新 SQL，验证局部实体更新不会附带清空三个提货字段。
     */
    @Test
    void partialEntityUpdateDoesNotClearPickupRules() {
        MybatisConfiguration configuration = new MybatisConfiguration();
        configuration.addMapper(GiftCardCashActivityMapper.class);
        GiftCardCashActivity partial = new GiftCardCashActivity();
        partial.setId("activity");
        partial.setGiftCardName("只修改名称");

        String sql = configuration.getMappedStatement(GiftCardCashActivityMapper.class.getName() + ".updateById")
                .getBoundSql(Map.of("et", partial)).getSql();

        assertThat(sql).contains("gift_card_name").doesNotContain("pickup_mode", "pickup_count", "pickup_sku_ids");
    }
}
