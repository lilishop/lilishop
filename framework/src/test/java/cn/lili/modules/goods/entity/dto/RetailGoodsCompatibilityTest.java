package cn.lili.modules.goods.entity.dto;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import com.alibaba.fastjson2.JSON;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * 验证零售模式对历史数据、旧请求和价格校验的兼容性。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
/**
 * 商品销售模式历史数据兼容测试，确保统一零售表现及价格校验。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
class RetailGoodsCompatibilityTest {

    /** 验证历史商品序列化时使用零售模式，且保留 SKU 价格。 */
    /**
     * 验证缺失、空值和历史销售模式统一按零售读写，且保留原商品价格。
     *
     * @param storedMode 历史存储或请求携带的销售模式
     */
    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"WHOLESALE", "RETAIL"})
    void legacyGoodsAndRequestsAreExposedAsRetail(String storedMode) {
        String json = JSON.toJSONString(storedMode == null ? Map.of("price", 99D)
                : Map.of("salesModel", storedMode, "price", 99D));
        Goods goods = JSON.parseObject(json, Goods.class);
        GoodsSku sku = JSON.parseObject(json, GoodsSku.class);
        GoodsOperationDTO request = JSON.parseObject(json, GoodsOperationDTO.class);

        assertThat(goods.getSalesModel()).isEqualTo("RETAIL");
        assertThat(sku.getSalesModel()).isEqualTo("RETAIL");
        assertThat(request.getSalesModel()).isEqualTo("RETAIL");
        assertThat(JSON.parseObject(JSON.toJSONString(sku)).getString("salesModel")).isEqualTo("RETAIL");
        assertThat(sku.getPrice()).isEqualTo(99D);
    }

    /** 验证旧请求中的销售模式不能绕过 SKU 售价和成本价校验。 */
    /**
     * 验证历史批发标记不能绕过 SKU 售价及成本价校验。
     *
     * @param price 售价，单位元
     * @param cost 成本价，单位元
     * @param error 预期业务错误码
     */
    @ParameterizedTest
    @CsvSource({"0,10,GOODS_SKU_PRICE_ERROR", "10,0,GOODS_SKU_COST_ERROR"})
    void legacySalesModeCannotBypassSkuPriceValidation(double price, double cost, ResultCode error) {
        GoodsOperationDTO request = new GoodsOperationDTO();
        request.setGoodsName("测试商品");
        request.setGoodsType("PHYSICAL_GOODS");
        request.setSalesModel("WHOLESALE");
        request.setSkuList(List.of(Map.of("price", price, "cost", cost)));

        assertThatThrownBy(() -> new Goods(request))
                .isInstanceOf(ServiceException.class)
                .hasMessage(error.message());
    }
}
