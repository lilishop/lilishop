package cn.lili.modules.search.serviceimpl;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import cn.lili.modules.search.service.GoodsIndexService;
import com.alibaba.fastjson2.JSON;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

@Service
public class GoodsIndexServiceImpl implements GoodsIndexService {

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    @Lazy
    private PromotionService promotionService;

    @Autowired
    @Lazy
    private PromotionGoodsService promotionGoodsService;

    @Override
    public GoodsIndex findById(String id) {
        GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(id);
        return buildGoodsIndex(goodsSku);
    }

    @Override
    public Map<String, Object> getPromotionMap(String id) {
        GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(id);
        return buildPromotionMap(goodsSku);
    }

    @Override
    public List<String> getPromotionIdByPromotionType(String id, PromotionTypeEnum promotionTypeEnum) {
        return Collections.emptyList();
    }

    @Override
    public GoodsIndex buildGoodsIndex(GoodsSku goodsSku) {
        if (goodsSku == null || Boolean.TRUE.equals(goodsSku.getDeleteFlag())) {
            return null;
        }
        GoodsIndex goodsIndex = new GoodsIndex(goodsSku);
        Map<String, Object> promotionMap = buildPromotionMap(goodsSku);
        goodsIndex.setPromotionMapJson(JSON.toJSONString(promotionMap));
        fillPromotionPrice(goodsIndex);
        return goodsIndex;
    }

    private Map<String, Object> buildPromotionMap(GoodsSku goodsSku) {
        if (goodsSku == null || promotionService == null) {
            return Collections.emptyMap();
        }
        Map<String, Object> promotionMap = promotionService.getGoodsSkuPromotionMap(goodsSku.getStoreId(), goodsSku.getId());
        return promotionMap == null ? Collections.emptyMap() : promotionMap;
    }

    private void fillPromotionPrice(GoodsIndex goodsIndex) {
        if (goodsIndex == null || promotionGoodsService == null) {
            return;
        }
        Double promotionPrice = promotionGoodsService.getValidPromotionsGoodsPrice(goodsIndex.getId(),
                Arrays.asList(PromotionTypeEnum.SECKILL.name(), PromotionTypeEnum.PINTUAN.name()));
        if (promotionPrice != null) {
            goodsIndex.setPromotionPrice(promotionPrice);
        }
    }
}
