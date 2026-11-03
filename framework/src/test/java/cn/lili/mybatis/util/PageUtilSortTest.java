package cn.lili.mybatis.util;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.search.utils.SqlFilter;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 分页排序字段校验与 SQL 关键字误判回归测试。
 *
 * @author Bulbasaur
 * @since 2026-11-03
 */
class PageUtilSortTest {
    @ParameterizedTest
    @ValueSource(strings = {"sort_order", "sortOrder", "buy_count", "create_time", "description",
            "commission", "scheduled_upper_time", "a.sort", "a.sort_order"})
    void acceptsColumnNamesContainingKeywordFragments(String sort) {
        PageVO params = new PageVO();
        params.setSort(sort);
        params.setOrder("ASC");

        Page<Object> page = PageUtil.initPage(params);

        assertThat(page.orders()).hasSize(1);
        assertThat(page.orders().get(0).getColumn()).isEqualTo(params.getSort());
        assertThat(page.orders().get(0).isAsc()).isTrue();
    }

    @ParameterizedTest
    @ValueSource(strings = {"id desc", "id,price", "rand()", "id;drop table users", "id--",
            "id/*comment*/", "id OR 1=1", "id/**/or/**/1=1", "'id'", "`id`", "id=1",
            "id\n", "id\t", "a..id", "a.id.extra", "id+1", "abcdefghijklmnopqrstu",
            "SELECT", "ORDER", "DESC"})
    void rejectsExpressionsAndInvalidOrReservedIdentifiers(String sort) {
        PageVO params = new PageVO();
        params.setSort(sort);
        params.setNotConvert(true);

        assertThat(PageUtil.initPage(params).orders()).isEmpty();
    }

    @Test
    void preservesDefaultDirectionAndPaginationLimits() {
        PageVO params = new PageVO();
        params.setSort("sort_order");
        params.setPageNumber(0);
        params.setPageSize(1000);

        Page<Object> page = PageUtil.initPage(params);

        assertThat(page.getCurrent()).isEqualTo(1);
        assertThat(page.getSize()).isEqualTo(100);
        assertThat(page.orders()).hasSize(1);
        assertThat(page.orders().get(0).isAsc()).isFalse();
    }

    @ParameterizedTest
    @ValueSource(strings = {"sort_order", "buy_count", "description", "commission", "create_time"})
    void keywordFilterDoesNotMatchPartsOfIdentifiers(String identifier) {
        assertThat(SqlFilter.hit(identifier)).isFalse();
    }

    @ParameterizedTest
    @ValueSource(strings = {"SELECT", "order by id", "id DESC", "DROP TABLE goods", "sleep(1)", "OR", "CREATE"})
    void keywordFilterStillMatchesSqlTokens(String expression) {
        assertThat(SqlFilter.hit(expression)).isTrue();
    }
}
