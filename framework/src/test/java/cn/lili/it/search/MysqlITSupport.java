package cn.lili.it.search;

import cn.lili.common.utils.SnowFlake;
import org.testcontainers.mysql.MySQLContainer;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

/**
 * 单例 MySQL 8.0.30，对齐 docker-compose。供 mapper / 倒排 / 分页 IT 共用，避免每用例拉镜像。
 *
 * @author Mike
 * @date 2026-09-03
 */
final class MysqlITSupport {

    static final String IMAGE = "mysql:8.0.30";

    static final MySQLContainer MYSQL = createAndStart();

    static {
        SnowFlake.initialize(1, 1);
    }

    private MysqlITSupport() {
    }

    private static MySQLContainer createAndStart() {
        MySQLContainer container = new MySQLContainer(IMAGE)
                .withDatabaseName("lilishop")
                .withUsername("root")
                .withPassword("lilishop");
        container.start();
        try (Connection connection = open(container)) {
            SearchPatchSql.executeSqlScript(connection, SearchPatchSql.readClasspath(SearchPatchSql.CLASSPATH_SCHEMA));
            SearchPatchSql.executeSqlScript(connection, SearchPatchSql.load004());
        } catch (SQLException e) {
            container.stop();
            throw new IllegalStateException("failed to apply search IT schema", e);
        }
        return container;
    }

    static String jdbcUrl() {
        return withMysqlParams(MYSQL.getJdbcUrl());
    }

    static String username() {
        return MYSQL.getUsername();
    }

    static String password() {
        return MYSQL.getPassword();
    }

    static Connection open() throws SQLException {
        return open(MYSQL);
    }

    static Connection open(MySQLContainer container) throws SQLException {
        return DriverManager.getConnection(withMysqlParams(container.getJdbcUrl()),
                container.getUsername(), container.getPassword());
    }

    static String withMysqlParams(String jdbcUrl) {
        String separator = jdbcUrl.contains("?") ? "&" : "?";
        return jdbcUrl + separator
                + "useSSL=false&allowPublicKeyRetrieval=true&allowMultiQueries=true&characterEncoding=utf8";
    }

    static void truncateSearchTables() {
        try (Connection connection = open(); Statement statement = connection.createStatement()) {
            statement.execute("TRUNCATE TABLE li_goods_search_param");
            statement.execute("TRUNCATE TABLE li_goods_sku");
            statement.execute("TRUNCATE TABLE li_goods");
            statement.execute("TRUNCATE TABLE li_brand");
        } catch (SQLException e) {
            throw new IllegalStateException("failed to truncate search IT tables", e);
        }
    }
}
