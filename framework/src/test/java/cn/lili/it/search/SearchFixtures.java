package cn.lili.it.search;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.sql.Types;
import java.time.LocalDateTime;

/**
 * 搜索 IT 的商品 / SKU / 品牌夹具。ID 用小整数，便于排序与翻页断言。
 *
 * @author Mike
 * @date 2026-09-03
 */
final class SearchFixtures {

    /**
     * 限制搜索测试数据工具类的实例化。
     */
    private SearchFixtures() {
    }

    /**
     * 构造单个参与索引的商品参数JSON。
     *
     * @param paramName  商品参数名称
     * @param paramValue 商品参数值
     * @param sort       排序字段
     * @return 单个参与索引的商品参数JSON
     */
    static String indexedJson(String paramName, String paramValue, int sort) {
        return "[{\"paramName\":\"" + paramName + "\",\"paramValue\":\"" + paramValue
                + "\",\"isIndex\":1,\"sort\":" + sort + "}]";
    }

    /**
     * 构造两个参与索引的商品参数JSON。
     *
     * @param name1  第一个参数名称
     * @param value1 第一个参数值
     * @param sort1  第一个参数排序值
     * @param name2  第二个参数名称
     * @param value2 第二个参数值
     * @param sort2  第二个参数排序值
     * @return 两个参与索引的商品参数JSON
     */
    static String indexedJsonTwo(String name1, String value1, int sort1,
                                 String name2, String value2, int sort2) {
        return "["
                + "{\"paramName\":\"" + name1 + "\",\"paramValue\":\"" + value1 + "\",\"isIndex\":1,\"sort\":" + sort1 + "},"
                + "{\"paramName\":\"" + name2 + "\",\"paramValue\":\"" + value2 + "\",\"isIndex\":1,\"sort\":" + sort2 + "}"
                + "]";
    }

    /**
     * 向测试数据库插入品牌数据。
     *
     * @param id   测试数据ID
     * @param name 名称
     * @param logo 品牌图标地址
     */
    static void insertBrand(String id, String name, String logo) {
        try (Connection connection = MysqlITSupport.open();
             PreparedStatement statement = connection.prepareStatement(
                     "INSERT INTO li_brand (id, delete_flag, name, logo) VALUES (?, 0, ?, ?)")) {
            statement.setLong(1, Long.parseLong(id));
            statement.setString(2, name);
            statement.setString(3, logo);
            statement.executeUpdate();
        } catch (SQLException e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * 向测试数据库插入指定参数及删除状态的商品。
     *
     * @param id         测试数据ID
     * @param params     商品参数JSON
     * @param deleted    删除标记
     * @param createTime 创建时间
     */
    static void insertGoods(String id, String params, boolean deleted, LocalDateTime createTime) {
        try (Connection connection = MysqlITSupport.open();
             PreparedStatement statement = connection.prepareStatement(
                     "INSERT INTO li_goods (id, create_time, delete_flag, params, goods_name) VALUES (?, ?, ?, ?, ?)")) {
            statement.setLong(1, Long.parseLong(id));
            statement.setTimestamp(2, Timestamp.valueOf(createTime));
            statement.setBoolean(3, deleted);
            statement.setString(4, params);
            statement.setString(5, "goods-" + id);
            statement.executeUpdate();
        } catch (SQLException e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * 更新测试数据库中的商品参数JSON。
     *
     * @param id     测试数据ID
     * @param params 商品参数JSON
     */
    static void updateGoodsParams(String id, String params) {
        try (Connection connection = MysqlITSupport.open();
             PreparedStatement statement = connection.prepareStatement(
                     "UPDATE li_goods SET params = ? WHERE id = ?")) {
            statement.setString(1, params);
            statement.setLong(2, Long.parseLong(id));
            statement.executeUpdate();
        } catch (SQLException e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * 插入审核通过且已上架的SKU测试数据。
     *
     * @param skuId        SKU ID
     * @param goodsId      商品ID
     * @param brandId      品牌ID
     * @param price        商品价格
     * @param buyCount     购买数量
     * @param commentNum   评价数量
     * @param categoryPath 分类路径
     */
    static void insertVisibleSku(String skuId, String goodsId, String brandId, BigDecimal price,
                                 Integer buyCount, Integer commentNum, String categoryPath) {
        insertSku(skuId, goodsId, brandId, price, null, buyCount, commentNum, categoryPath,
                "UPPER", "PASS", false);
    }

    /**
     * 插入包含成本价的可售SKU测试数据。
     *
     * @param skuId        SKU ID
     * @param goodsId      商品ID
     * @param brandId      品牌ID
     * @param price        商品价格
     * @param cost         成本价
     * @param buyCount     购买数量
     * @param commentNum   评价数量
     * @param categoryPath 分类路径
     */
    static void insertVisibleSkuWithCost(String skuId, String goodsId, String brandId, BigDecimal price,
                                         BigDecimal cost, Integer buyCount, Integer commentNum, String categoryPath) {
        insertSku(skuId, goodsId, brandId, price, cost, buyCount, commentNum, categoryPath,
                "UPPER", "PASS", false);
    }

    /**
     * 向测试数据库插入指定库存销售状态的SKU。
     *
     * @param skuId        SKU ID
     * @param goodsId      商品ID
     * @param brandId      品牌ID
     * @param price        商品价格
     * @param buyCount     购买数量
     * @param commentNum   评价数量
     * @param categoryPath 分类路径
     * @param marketEnable 商品上下架状态
     * @param authFlag     认证开关
     * @param deleted      删除标记
     */
    static void insertSku(String skuId, String goodsId, String brandId, BigDecimal price,
                          Integer buyCount, Integer commentNum, String categoryPath,
                          String marketEnable, String authFlag, boolean deleted) {
        insertSku(skuId, goodsId, brandId, price, null, buyCount, commentNum, categoryPath,
                marketEnable, authFlag, deleted);
    }

    /**
     * 向测试数据库插入指定库存销售状态的SKU。
     *
     * @param skuId        SKU ID
     * @param goodsId      商品ID
     * @param brandId      品牌ID
     * @param price        商品价格
     * @param cost         成本价
     * @param buyCount     购买数量
     * @param commentNum   评价数量
     * @param categoryPath 分类路径
     * @param marketEnable 商品上下架状态
     * @param authFlag     认证开关
     * @param deleted      删除标记
     */
    static void insertSku(String skuId, String goodsId, String brandId, BigDecimal price, BigDecimal cost,
                          Integer buyCount, Integer commentNum, String categoryPath,
                          String marketEnable, String authFlag, boolean deleted) {
        String sql = "INSERT INTO li_goods_sku ("
                + "id, create_time, delete_flag, brand_id, buy_count, virtual_sales, category_path, comment_num, "
                + "goods_id, goods_name, auth_flag, market_enable, price, cost, recommend, sales_model, store_id, goods_type"
                + ") VALUES (?, NOW(6), ?, ?, ?, 0, ?, ?, ?, ?, ?, ?, ?, ?, 0, 'RETAIL', 'store-1', 'PHYSICAL_GOODS')";
        try (Connection connection = MysqlITSupport.open();
             PreparedStatement statement = connection.prepareStatement(sql)) {
            statement.setLong(1, Long.parseLong(skuId));
            statement.setBoolean(2, deleted);
            statement.setString(3, brandId);
            if (buyCount == null) {
                statement.setNull(4, Types.INTEGER);
            } else {
                statement.setInt(4, buyCount);
            }
            statement.setString(5, categoryPath);
            if (commentNum == null) {
                statement.setNull(6, Types.INTEGER);
            } else {
                statement.setInt(6, commentNum);
            }
            statement.setString(7, goodsId);
            statement.setString(8, "sku-" + skuId);
            statement.setString(9, authFlag);
            statement.setString(10, marketEnable);
            if (price == null) {
                statement.setNull(11, Types.DECIMAL);
            } else {
                statement.setBigDecimal(11, price);
            }
            if (cost == null) {
                statement.setNull(12, Types.DECIMAL);
            } else {
                statement.setBigDecimal(12, cost);
            }
            statement.executeUpdate();
        } catch (SQLException e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * 统计测试数据库中的参数倒排记录数。
     *
     * @param goodsId 商品ID
     * @return 测试数据库中的参数倒排记录数
     */
    static int countSearchParams(String goodsId) {
        try (Connection connection = MysqlITSupport.open();
             PreparedStatement statement = connection.prepareStatement(
                     "SELECT COUNT(*) FROM li_goods_search_param WHERE goods_id = ?")) {
            statement.setString(1, goodsId);
            try (ResultSet resultSet = statement.executeQuery()) {
                resultSet.next();
                return resultSet.getInt(1);
            }
        } catch (SQLException e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * 统计测试数据库中的参数倒排记录数。
     *
     * @return 测试数据库中的参数倒排记录数
     */
    static int countSearchParams() {
        try (Connection connection = MysqlITSupport.open();
             PreparedStatement statement = connection.prepareStatement(
                     "SELECT COUNT(*) FROM li_goods_search_param");
             ResultSet resultSet = statement.executeQuery()) {
            resultSet.next();
            return resultSet.getInt(1);
        } catch (SQLException e) {
            throw new IllegalStateException(e);
        }
    }

    /**
     * 判断指定商品的参数名称和值是否已写入倒排表。
     *
     * @param goodsId    商品ID
     * @param paramName  商品参数名称
     * @param paramValue 商品参数值
     * @return 是否存在匹配的参数倒排记录
     */
    static boolean searchParamExists(String goodsId, String paramName, String paramValue) {
        try (Connection connection = MysqlITSupport.open();
             PreparedStatement statement = connection.prepareStatement(
                     "SELECT COUNT(*) FROM li_goods_search_param WHERE goods_id = ? AND param_name = ? AND param_value = ?")) {
            statement.setString(1, goodsId);
            statement.setString(2, paramName);
            statement.setString(3, paramValue);
            try (ResultSet resultSet = statement.executeQuery()) {
                resultSet.next();
                return resultSet.getInt(1) > 0;
            }
        } catch (SQLException e) {
            throw new IllegalStateException(e);
        }
    }
}
