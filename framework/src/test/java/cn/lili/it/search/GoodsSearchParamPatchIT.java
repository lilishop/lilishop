package cn.lili.it.search;

import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * 倒排表建表脚本在 MySQL 8 与 MariaDB 上均可执行，且 goods_id 排序规则与 SKU 对齐。
 *
 * @author Mike
 * @date 2026-09-03
 */
@Tag("it")
class GoodsSearchParamPatchIT {

    @Test
    void dbPatchIsLoadable() {
        String patch = SearchPatchSql.load004();
        assertThat(patch).contains("li_goods_search_param");
        assertThat(SearchPatchSql.findDbPatch()).isNotNull();
    }

    @Test
    void patch004IsIdempotentOnMysql8WithBaseTablesPresent() throws Exception {
        String patch = SearchPatchSql.load004();
        try (Connection connection = MysqlITSupport.open()) {
            SearchPatchSql.executeSqlScript(connection, patch);
            SearchPatchSql.executeSqlScript(connection, patch);
            assertSearchParamTable(connection);
        }
    }

    @Test
    void patch004AppliesOnMariaDbBeforeAndAfterBaseTables() throws Exception {
        String patch = SearchPatchSql.load004();
        try (Connection connection = MariaDbITSupport.open()) {
            SearchPatchSql.executeSqlScript(connection, patch);
            assertSearchParamTable(connection);

            SearchPatchSql.executeSqlScript(connection, SearchPatchSql.readClasspath(SearchPatchSql.CLASSPATH_SCHEMA));
            SearchPatchSql.executeSqlScript(connection, patch);
            assertSearchParamTable(connection);
        }
    }

    private static void assertSearchParamTable(Connection connection) throws Exception {
        try (Statement statement = connection.createStatement()) {
            try (ResultSet tables = statement.executeQuery(
                    "SELECT COUNT(*) FROM information_schema.TABLES "
                            + "WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_goods_search_param'")) {
                tables.next();
                assertThat(tables.getInt(1)).isEqualTo(1);
            }
            try (ResultSet column = statement.executeQuery(
                    "SELECT CHARACTER_SET_NAME, COLLATION_NAME FROM information_schema.COLUMNS "
                            + "WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_goods_search_param' "
                            + "AND COLUMN_NAME = 'goods_id'")) {
                assertThat(column.next()).isTrue();
                assertThat(column.getString("CHARACTER_SET_NAME")).isIn("utf8", "utf8mb3");
                assertThat(column.getString("COLLATION_NAME")).isIn("utf8mb3_general_ci", "utf8_general_ci");
            }
            try (ResultSet uniqueKey = statement.executeQuery(
                    "SELECT 1 FROM information_schema.STATISTICS "
                            + "WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_goods_search_param' "
                            + "AND INDEX_NAME = 'uk_li_goods_search_param_goods_name_value' LIMIT 1")) {
                assertThat(uniqueKey.next()).isTrue();
            }
            try (ResultSet lookup = statement.executeQuery(
                    "SELECT 1 FROM information_schema.STATISTICS "
                            + "WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'li_goods_search_param' "
                            + "AND INDEX_NAME = 'idx_li_goods_search_param_lookup' LIMIT 1")) {
                assertThat(lookup.next()).isTrue();
            }
        }
    }
}
