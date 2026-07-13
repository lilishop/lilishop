package cn.lili.modules.search.service;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.search.entity.dos.GoodsIndex;

import java.util.List;
import java.util.Map;

/**
 * 商品搜索数据服务（运行时从 MySQL/缓存组装，不再维护 ES 索引）
 */
public interface GoodsIndexService {

    /**
     * 根据 id 获取商品信息
     */
    GoodsIndex findById(String id);

    /**
     * 根据 id 获取商品促销信息
     */
    Map<String, Object> getPromotionMap(String id);

    /**
     * 根据 id 获取商品指定促销类型的促销 id 列表
     */
    List<String> getPromotionIdByPromotionType(String id, PromotionTypeEnum promotionTypeEnum);

    /**
     * 从 SKU 组装商品搜索数据
     */
    GoodsIndex buildGoodsIndex(GoodsSku goodsSku);
}
