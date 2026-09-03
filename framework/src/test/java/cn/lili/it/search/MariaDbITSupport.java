package cn.lili.it.search;

import org.testcontainers.mariadb.MariaDBContainer;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

/**
 * 单例 MariaDB 10.11，对齐 all-in-one 内置库，只用于 004 补丁可执行性。
 *
 * @author Mike
 * @date 2026-09-03
 */
final class MariaDbITSupport {

    static final String IMAGE = "mariadb:10.11";

    static final MariaDBContainer MARIADB = createAndStart();

    private MariaDbITSupport() {
    }

    private static MariaDBContainer createAndStart() {
        MariaDBContainer container = new MariaDBContainer(IMAGE)
                .withDatabaseName("lilishop")
                .withUsername("root")
                .withPassword("lilishop");
        container.start();
        return container;
    }

    static Connection open() throws SQLException {
        String jdbcUrl = MARIADB.getJdbcUrl();
        String separator = jdbcUrl.contains("?") ? "&" : "?";
        // 用 MySQL 驱动连 MariaDB 协议，避免再引入 mariadb-java-client。
        String mysqlUrl = jdbcUrl.replace("jdbc:mariadb:", "jdbc:mysql:")
                + separator + "useSSL=false&allowPublicKeyRetrieval=true&allowMultiQueries=true";
        return DriverManager.getConnection(mysqlUrl, MARIADB.getUsername(), MARIADB.getPassword());
    }
}
