package cn.lili.modules.search.serviceimpl;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.promotion.entity.dos.BasePromotions;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.modules.goods.service.GoodsSkuService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;
import java.util.Map;

@Service
@ConditionalOnProperty(value = "lili.data.elasticsearch.enabled", havingValue = "false")
public class EsGoodsIndexServiceNoop implements EsGoodsIndexService {

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    public Boolean deleteGoodsDown() {
        return true;
    }

    @Override
    public Boolean delSkuIndex() {
        return true;
    }

    @Override
    public Boolean goodsCache() {
        return true;
    }

    @Override
    public void init() {
    }

    @Override
    public Map<String, Long> getProgress() {
        return Collections.emptyMap();
    }

    @Override
    public <T> T findIndexById(String id, Class<T> clazz) {
        return null;
    }

    @Override
    public EsGoodsIndex findEsGoodsIndexById(String id) {
        return findById(id);
    }

    @Override
    public EsGoodsIndex addIndex(EsGoodsIndex goods) {
        return goods;
    }

    @Override
    public void addIndex(List<EsGoodsIndex> goods) {
    }

    @Override
    public void updateIndex(EsGoodsIndex goods) {
    }

    @Override
    public void updateIndex(String id, EsGoodsIndex goods) {
    }

    @Override
    public void updateIndex(Map<String, Object> queryFields, Map<String, Object> updateFields) {
    }

    @Override
    public void deleteIndexById(String id) {
    }

    @Override
    public void deleteIndexByIds(List<String> ids) {
    }

    @Override
    public void initIndex(List<EsGoodsIndex> goodsIndexList, boolean regeneratorIndex) {
    }

    @Override
    public void updateEsGoodsIndexPromotions(List<String> ids, BasePromotions promotion, String key) {
    }

    @Override
    public void updateEsGoodsIndexByList(List<PromotionGoods> promotionGoodsList, BasePromotions promotion, String key) {
    }

    @Override
    public void updateEsGoodsIndexAllByList(BasePromotions promotion, String key) {
    }

    @Override
    public void deleteEsGoodsPromotionByPromotionKey(List<String> skuIds, String promotionsKey) {
    }

    @Override
    public void deleteEsGoodsPromotionByPromotionKey(String promotionsKey) {
    }

    @Override
    public void cleanInvalidPromotion() {
    }

    @Override
    public EsGoodsIndex findById(String id) {
        GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(id);
        return getResetEsGoodsIndex(goodsSku);
    }

    @Override
    public Map<String, Object> getPromotionMap(String id) {
        return Collections.emptyMap();
    }

    @Override
    public List<String> getPromotionIdByPromotionType(String id, PromotionTypeEnum promotionTypeEnum) {
        return Collections.emptyList();
    }

    @Override
    public EsGoodsIndex getResetEsGoodsIndex(GoodsSku goodsSku) {
        if (goodsSku == null || Boolean.TRUE.equals(goodsSku.getDeleteFlag())) {
            return null;
        }
        EsGoodsIndex goodsIndex = new EsGoodsIndex(goodsSku);
        goodsIndex.setPromotionMapJson("{}");
        return goodsIndex;
    }
}
