package cn.lili.event.impl;


import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.event.GoodsCommentCompleteEvent;
import cn.lili.event.StoreSettingChangeEvent;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.store.entity.dos.Store;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

/**
 * 商品SKU变化
 *
 * @author Chopper
 * @since 2020-07-03 11:20
 */
@Service
public class GoodsSkuExecute implements GoodsCommentCompleteEvent, StoreSettingChangeEvent {

    /**
     * 商品
     */
    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    private GoodsService goodsService;

    @Autowired
    private Cache cache;

    @Override
    public void goodsComment(MemberEvaluation memberEvaluation) {
        goodsService.updateGoodsCommentNum(memberEvaluation.getGoodsId(), memberEvaluation.getSkuId());
    }

    @Override
    public void storeSettingChange(Store store) {
        //修改数据后，清除商品索引
        GoodsSearchParams goodsSearchParams = new GoodsSearchParams();
        goodsSearchParams.setStoreId(store.getId());
        List<String> goodsSkuKeys = new ArrayList<>();
        for (GoodsSku goodsSku : goodsSkuService.getGoodsSkuByList(goodsSearchParams)) {
            goodsSkuKeys.add(CachePrefix.GOODS_SKU.getPrefix()+goodsSku.getId());
        }
        cache.multiDel(goodsSkuKeys);
    }
}
