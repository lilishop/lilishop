package cn.lili.modules.search.utils;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

/**
 * sql 关键字过滤
 *
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.0
 * @Description:
 * @since 2023/3/22 17:27
 */

public class SqlFilter {

    private static final Set<String> SQL_KEYWORDS = new HashSet<>(Arrays.asList(
            "SELECT", "FROM", "WHERE", "AND", "OR", "NOT", "INSERT", "UPDATE", "DELETE", "CREATE",
            "TABLE", "INDEX", "VIEW", "DROP", "ALTER", "COLUMN", "ADD", "SET", "GROUP", "BY",
            "HAVING", "ORDER", "ASC", "DESC", "LIKE", "IN", "BETWEEN", "IS", "NULL", "TRUE", "FALSE",
            "JOIN", "LEFT", "RIGHT", "INNER", "OUTER", "FULL", "ON", "AS", "DISTINCT", "COUNT",
            "MAX", "MIN", "SUM", "AVG"
    ));


    /**
     * 关键字命中
     *
     * @param sql
     * @return
     */
    public static Boolean hit(String sql) {
        String[] tokens = sql.split("\\s+");
        for (String token : tokens) {
            if (!SQL_KEYWORDS.contains(token.toUpperCase())) {
                return true;
            }
        }
        return false;
    }

    /**
     * 关键字替换
     *
     * @param sql
     * @return
     */
    public static String filterSql(String sql) {
        String[] tokens = sql.split("\\s+");
        StringBuilder filteredSql = new StringBuilder();
        for (String token : tokens) {
            if (!SQL_KEYWORDS.contains(token.toUpperCase())) {
                filteredSql.append(token).append(" ");
            }
        }
        return filteredSql.toString().trim();
    }
}
