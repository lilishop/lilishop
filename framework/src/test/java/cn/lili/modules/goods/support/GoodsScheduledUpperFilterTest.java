package cn.lili.modules.goods.support;

import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 定时上架商品筛选条件测试。
 *
 * @author Bulbasaur
 * @since 2026-11-03
 */
class GoodsScheduledUpperFilterTest {
    @Test
    void scheduledListIncludesPendingPlansEvenWhenTheirTimeHasPassed() {
        GoodsSearchParams params = new GoodsSearchParams();
        params.setGoodsStatus("SCHEDULED_UPPER");
        assertThat(params.queryWrapper().getSqlSegment())
                .contains("scheduled_upper_time IS NOT NULL", "delete_flag =")
                .doesNotContain("scheduled_upper_time >", "scheduled_upper_time <");
    }

    @Test
    void sellerScheduledListKeepsStoreAndSearchFilters() {
        GoodsSearchParams params = new GoodsSearchParams();
        params.setGoodsStatus("SCHEDULED_UPPER");
        params.setStoreId("store-1");
        params.setGoodsName("测试商品");
        QueryWrapper<Object> wrapper = params.queryWrapper();
        assertThat(wrapper.getSqlSegment()).contains("store_id =", "goods_name LIKE", "scheduled_upper_time IS NOT NULL");
        assertThat(wrapper.getParamNameValuePairs()).containsValue("store-1").containsValue("%测试商品%");
    }

    @Test
    void existingStatusQueriesDoNotRequireScheduledPlans() {
        GoodsSearchParams params = new GoodsSearchParams();
        assertThat(params.queryWrapper().getSqlSegment()).doesNotContain("scheduled_upper_time");
        params.setGoodsStatus("DOWN");
        assertThat(params.queryWrapper().getSqlSegment()).contains("market_enable =", "auth_flag =").doesNotContain("scheduled_upper_time");
    }
}
