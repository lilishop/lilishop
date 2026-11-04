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
    /**
     * 验证排序允许包含SQL关键字片段的合法字段名。
     *
     * @param sort 排序字段
     */
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

    /**
     * 验证排序拒绝表达式、非法标识符及保留字。
     *
     * @param sort 排序字段
     */
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

    /**
     * 验证排序处理保留默认方向及分页边界限制。
     */
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

    /**
     * 验证SQL关键字检测不误判标识符片段。
     *
     * @param identifier 容器标识
     */
    @ParameterizedTest
    @ValueSource(strings = {"sort_order", "buy_count", "description", "commission", "create_time"})
    void keywordFilterDoesNotMatchPartsOfIdentifiers(String identifier) {
        assertThat(SqlFilter.hit(identifier)).isFalse();
    }

    /**
     * 验证SQL关键字检测仍识别独立SQL标记。
     *
     * @param expression 查询表达式
     */
    @ParameterizedTest
    @ValueSource(strings = {"SELECT", "order by id", "id DESC", "DROP TABLE goods", "sleep(1)", "OR", "CREATE"})
    void keywordFilterStillMatchesSqlTokens(String expression) {
        assertThat(SqlFilter.hit(expression)).isTrue();
    }
}
