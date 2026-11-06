package cn.lili.modules.promotion.serviceimpl;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dto.GiftCardCashActivityDTO;
import cn.lili.modules.promotion.entity.vos.PickupCardSkuVO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * 提货规则拆分回归测试，验证活动归一化及失效、缺货商品的展示口径不变。
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
class PickupCardRuleServiceImplTest {

    private PickupCardRuleServiceImpl rules;
    private GoodsSkuService goods;

    /** 仅模拟商品查询边界，规则使用真实实现。 */
    @BeforeEach
    void setUp() {
        rules = new PickupCardRuleServiceImpl();
        goods = mock(GoodsSkuService.class);
        ReflectionTestUtils.setField(rules, "goodsSkuService", goods);
        GoodsSku sku = new GoodsSku();
        sku.setId("sku");
        sku.setGoodsName("候选商品");
        sku.setAuthFlag("PASS");
        sku.setMarketEnable("UPPER");
        sku.setGoodsType("PHYSICAL_GOODS");
        sku.setPrice(12.5D);
        when(goods.getById("sku")).thenReturn(sku);
        when(goods.getById("sku2")).thenReturn(sku);
    }

    /** 三种模式的兑换种数归一化及零面额规则与拆分前一致。 */
    @ParameterizedTest
    @CsvSource({"ONE,99,1", "ALL,99,2", "ANY,2,2"})
    void normalizesActivityWithoutOrderServices(String mode, int inputCount, int expectedCount) {
        GiftCardCashActivityDTO request = new GiftCardCashActivityDTO();
        request.setPickupMode(mode);
        request.setPickupCount(inputCount);
        request.setPickupSkuIds(List.of("sku", "sku2"));
        request.setFaceValue(BigDecimal.TEN);

        rules.validateActivity(request);

        assertThat(request.getPickupCount()).isEqualTo(expectedCount);
        assertThat(request.getFaceValue()).isEqualByComparingTo(BigDecimal.ZERO);
    }

    /** 失效商品保留原 SKU 占位，缺货商品保留展示但不可选。 */
    @Test
    void preservesUnavailableGoodsPlaceholders() {
        when(goods.getStock("sku")).thenReturn(0);

        List<PickupCardSkuVO> result = rules.goods("[\"removed\",\"sku\"]");

        assertThat(result).extracting(PickupCardSkuVO::getId).containsExactly("removed", "sku");
        assertThat(result).extracting(PickupCardSkuVO::getAvailable).containsExactly(false, false);
        assertThat(result.get(0).getGoodsName()).contains("已失效");
        assertThat(result.get(1).getPrice()).isEqualTo(12.5D);
    }
}
