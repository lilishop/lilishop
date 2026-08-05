package cn.lili.modules.distribution.mapper;

import cn.lili.modules.distribution.entity.vos.DistributionFeaturedGoodsItemVO;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 分销主推商品查询
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionFeaturedGoodsMapper {

    /**
     * 按佣金从高到低自动匹配主推商品。
     */
    @Select("SELECT dg.id, dg.goods_id AS goodsId, dg.sku_id AS skuId, dg.goods_name AS goodsName, dg.specs, "
            + "dg.thumbnail, dg.price, dg.commission, IFNULL(gs.buy_count, 0) AS salesCount "
            + "FROM li_distribution_goods dg "
            + "LEFT JOIN li_goods_sku gs ON dg.sku_id = gs.id AND gs.delete_flag = 0 "
            + "WHERE dg.auth_status = 'PASS' ORDER BY dg.commission DESC LIMIT #{limit}")
    List<DistributionFeaturedGoodsItemVO> listAutoByCommission(@Param("limit") int limit);

    /**
     * 按销量从高到低自动匹配主推商品。
     */
    @Select("SELECT dg.id, dg.goods_id AS goodsId, dg.sku_id AS skuId, dg.goods_name AS goodsName, dg.specs, "
            + "dg.thumbnail, dg.price, dg.commission, IFNULL(gs.buy_count, 0) AS salesCount "
            + "FROM li_distribution_goods dg "
            + "LEFT JOIN li_goods_sku gs ON dg.sku_id = gs.id AND gs.delete_flag = 0 "
            + "WHERE dg.auth_status = 'PASS' "
            + "ORDER BY IFNULL(gs.buy_count, 0) DESC, dg.commission DESC LIMIT #{limit}")
    List<DistributionFeaturedGoodsItemVO> listAutoBySales(@Param("limit") int limit);

    /**
     * 按 SKU ID 列表查询自定义主推商品。
     */
    @Select("<script>"
            + "SELECT dg.id, dg.goods_id AS goodsId, dg.sku_id AS skuId, dg.goods_name AS goodsName, dg.specs, "
            + "dg.thumbnail, dg.price, dg.commission, IFNULL(gs.buy_count, 0) AS salesCount "
            + "FROM li_distribution_goods dg "
            + "LEFT JOIN li_goods_sku gs ON dg.sku_id = gs.id AND gs.delete_flag = 0 "
            + "WHERE dg.auth_status = 'PASS' AND dg.sku_id IN "
            + "<foreach collection='skuIds' item='skuId' open='(' separator=',' close=')'>#{skuId}</foreach>"
            + "</script>")
    List<DistributionFeaturedGoodsItemVO> listBySkuIds(@Param("skuIds") List<String> skuIds);
}
