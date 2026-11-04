package cn.lili.it.search;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

/**
 * 定位并执行倒排表建表脚本：源文件为仓库 {@code DB/li_goods_search_param.sql}。
 *
 * @author Mike
 * @date 2026-09-03
 */
final class SearchPatchSql {

    static final String CLASSPATH_SCHEMA = "/it/search-schema.sql";
    private static final String RELATIVE_PATCH = "DB/li_goods_search_param.sql";

    /**
     * 限制搜索数据库补丁工具类的实例化。
     */
    private SearchPatchSql() {
    }

    /**
     * 加载商品参数倒排表的004数据库补丁。
     *
     * @return 商品参数倒排表的004数据库补丁
     */
    static String load004() {
        Path file = findDbPatch();
        if (file == null) {
            throw new IllegalStateException("missing " + RELATIVE_PATCH
                    + " (walked up from " + Path.of(System.getProperty("user.dir", ".")).toAbsolutePath() + ")");
        }
        try {
            return Files.readString(file);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * 在项目目录中定位指定数据库补丁资源。
     *
     * @return 数据库补丁文件路径
     */
    static Path findDbPatch() {
        Path current = Path.of(System.getProperty("user.dir", ".")).toAbsolutePath().normalize();
        for (int i = 0; i < 6 && current != null; i++) {
            Path candidate = current.resolve(RELATIVE_PATCH);
            if (Files.isRegularFile(candidate)) {
                return candidate;
            }
            current = current.getParent();
        }
        return null;
    }

    /**
     * 从类路径读取SQL补丁文本。
     *
     * @param resource 资源名称
     * @return SQL补丁文本
     */
    static String readClasspath(String resource) {
        try (InputStream in = SearchPatchSql.class.getResourceAsStream(resource)) {
            if (in == null) {
                throw new IllegalStateException("missing classpath resource " + resource);
            }
            return new String(in.readAllBytes(), StandardCharsets.UTF_8);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * 拆分并逐条执行SQL补丁语句。
     *
     * @param connection 数据库连接
     * @param script     SQL脚本文本
     */
    static void executeSqlScript(Connection connection, String script) throws SQLException {
        for (String statement : splitStatements(script)) {
            try (Statement st = connection.createStatement()) {
                st.execute(statement);
            }
        }
    }

    /**
     * 按SQL语句边界拆分脚本并处理注释及引号内容。
     *
     * @param script SQL脚本文本
     * @return 拆分后的SQL语句列表
     */
    static List<String> splitStatements(String script) {
        List<String> statements = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        for (String line : script.split("\\R")) {
            String trimmed = line.trim();
            if (trimmed.startsWith("--") || trimmed.isEmpty()) {
                continue;
            }
            current.append(line).append('\n');
            if (trimmed.endsWith(";")) {
                String sql = current.toString().trim();
                if (sql.endsWith(";")) {
                    sql = sql.substring(0, sql.length() - 1).trim();
                }
                if (!sql.isEmpty()) {
                    statements.add(sql);
                }
                current.setLength(0);
            }
        }
        String tail = current.toString().trim();
        if (!tail.isEmpty()) {
            statements.add(tail.endsWith(";") ? tail.substring(0, tail.length() - 1).trim() : tail);
        }
        return statements;
    }
}
