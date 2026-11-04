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

    /**
     * 限制MySQL集成测试工具类的实例化。
     */
    private MysqlITSupport() {
    }

    /**
     * 创建并启动集成测试使用的数据库容器。
     *
     * @return 已启动的测试数据库容器
     */
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

    /**
     * 获取附加测试连接参数的MySQL JDBC地址。
     *
     * @return 附加测试连接参数的MySQL JDBC地址
     */
    static String jdbcUrl() {
        return withMysqlParams(MYSQL.getJdbcUrl());
    }

    /**
     * 获取测试数据库容器的用户名。
     *
     * @return 测试数据库容器的用户名
     */
    static String username() {
        return MYSQL.getUsername();
    }

    /**
     * 获取测试数据库容器的密码。
     *
     * @return 测试数据库容器的密码
     */
    static String password() {
        return MYSQL.getPassword();
    }

    /**
     * 使用测试数据库配置创建JDBC连接。
     *
     * @return 测试数据库连接
     */
    static Connection open() throws SQLException {
        return open(MYSQL);
    }

    /**
     * 使用测试数据库配置创建JDBC连接。
     *
     * @param container 数据库容器
     * @return 测试数据库连接
     */
    static Connection open(MySQLContainer container) throws SQLException {
        return DriverManager.getConnection(withMysqlParams(container.getJdbcUrl()),
                container.getUsername(), container.getPassword());
    }

    /**
     * 向JDBC地址追加集成测试所需的MySQL连接参数。
     *
     * @param jdbcUrl 数据库连接地址
     * @return 追加测试连接参数后的JDBC地址
     */
    static String withMysqlParams(String jdbcUrl) {
        String separator = jdbcUrl.contains("?") ? "&" : "?";
        return jdbcUrl + separator
                + "useSSL=false&allowPublicKeyRetrieval=true&allowMultiQueries=true&characterEncoding=utf8";
    }

    /**
     * 清空商品搜索相关测试表以隔离集成测试数据。
     */
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
