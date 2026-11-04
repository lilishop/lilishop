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
    /**
     * 验证定时下架列表保留店铺及未删除条件，并包含到期但尚未清除的下架计划
     */
    @Test
    void scheduledDownListKeepsStoreFilterAndIncludesOverduePlans() {
        GoodsSearchParams params = new GoodsSearchParams();
        params.setGoodsStatus("SCHEDULED_DOWN");
        params.setStoreId("store-1");
        QueryWrapper<Object> wrapper = params.queryWrapper();
        assertThat(wrapper.getSqlSegment()).contains("scheduled_down_time IS NOT NULL", "store_id =", "delete_flag =")
                .doesNotContain("scheduled_upper_time", "scheduled_down_time >", "scheduled_down_time <");
        assertThat(wrapper.getParamNameValuePairs()).containsValue("store-1");
    }

    /**
     * 验证定时上架列表按计划是否存在筛选，不排除已到期但尚未清除的计划
     */
    @Test
    void scheduledListIncludesPendingPlansEvenWhenTheirTimeHasPassed() {
        GoodsSearchParams params = new GoodsSearchParams();
        params.setGoodsStatus("SCHEDULED_UPPER");
        assertThat(params.queryWrapper().getSqlSegment())
                .contains("scheduled_upper_time IS NOT NULL", "delete_flag =")
                .doesNotContain("scheduled_upper_time >", "scheduled_upper_time <");
    }

    /**
     * 验证定时上架筛选与店铺范围、商品名称搜索条件同时生效
     */
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

    /**
     * 验证普通商品查询不附加定时计划条件，仓库筛选仍保留审核及上下架状态条件
     */
    @Test
    void existingStatusQueriesDoNotRequireScheduledPlans() {
        GoodsSearchParams params = new GoodsSearchParams();
        assertThat(params.queryWrapper().getSqlSegment()).doesNotContain("scheduled_upper_time");
        params.setGoodsStatus("DOWN");
        assertThat(params.queryWrapper().getSqlSegment()).contains("market_enable =", "auth_flag =").doesNotContain("scheduled_upper_time");
    }
}
