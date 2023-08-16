package cn.lili.modules.search.utils;

import cn.lili.common.utils.StringUtils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * sql 关键字过滤
 *
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.0
 * @Description:
 * @since 2023/3/22 17:27
 */

public class SqlFilter {


    // SQL注入过滤
    static final String SQL_KEYWORDS_PATTERN =
            "(?i)(SELECT|FROM|WHERE|CONCAT|AND|OR|NOT|INSERT|UPDATE|DELETE|CREATE" +
            "|TABLE|INDEX|VIEW|DROP|ALTER|COLUMN|ADD|SET|GROUP|BY" +
            "|HAVING|ORDER|ASC|DESC|LIKE|IN|BETWEEN|IS|NULL|TRUE|FALSE" +
            "|JOIN|LEFT|RIGHT|INNER|OUTER|FULL|ON|AS|DISTINCT|COUNT" +
            "|MAX|MIN|SUM|AVG|IF|RAND|UPDATEXML|EXTRACTVALUE|LOAD_FILE|SLEEP|OFFSET)";
    static final Pattern keywordPattern = Pattern.compile(SQL_KEYWORDS_PATTERN, Pattern.CASE_INSENSITIVE);


    /**
     * 关键字命中
     *
     * @param sql
     * @return
     */
    public static Boolean hit(String sql) {
        if (StringUtils.isEmpty(sql)) {
            return false;
        }
        Matcher matcher = keywordPattern.matcher(sql);
        return matcher.find();
    }


}
