package cn.lili.modules.goods.support;

import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 商品虚拟销量筛选条件测试。
 *
 * @author Bulbasaur
 * @since 2026-11-03
 */
class GoodsVirtualSalesFilterTest {
    @Test
    void allDoesNotFilterVirtualSales() {
        assertThat(new GoodsSearchParams().queryWrapper().getSqlSegment()).doesNotContain("virtual_sales");
    }

    @Test
    void configuredRequiresPositiveVirtualSales() {
        GoodsSearchParams params = new GoodsSearchParams();
        params.setVirtualSalesSet(true);
        QueryWrapper<Object> wrapper = params.queryWrapper();
        assertThat(wrapper.getSqlSegment()).contains("virtual_sales >");
        assertThat(wrapper.getParamNameValuePairs()).containsValue(0);
    }

    @Test
    void unconfiguredIncludesZeroAndNullWithinOtherFilters() {
        GoodsSearchParams params = new GoodsSearchParams();
        params.setVirtualSalesSet(false);
        params.setGoodsName("测试商品");
        QueryWrapper<Object> wrapper = params.queryWrapper();
        assertThat(wrapper.getSqlSegment()).contains("goods_name LIKE", "AND (virtual_sales =", "OR virtual_sales IS NULL)");
        assertThat(wrapper.getParamNameValuePairs()).containsValue(0).containsValue("%测试商品%");
    }
}
