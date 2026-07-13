package cn.lili.listener;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.wxchannels.entity.dos.WxChannelGoods;
import cn.lili.modules.wxchannels.service.WxChannelGoodsService;
import cn.lili.modules.wxchannels.service.WxChannelsApiService;
import cn.lili.message.QueueMessage;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

@Component
@Slf4j
public class WxChannelsGoodsSyncListener {

    @Autowired
    private GoodsService goodsService;
    @Autowired
    private GoodsSkuService goodsSkuService;
    @Autowired
    private CategoryService categoryService;
    @Autowired
    private WxChannelGoodsService wxChannelGoodsService;
    @Autowired
    private WxChannelsApiService wxChannelsApiService;

    public void onMessage(QueueMessage messageExt) {
        GoodsTagsEnum tag = GoodsTagsEnum.valueOf(messageExt.getTags());
        switch (tag) {
            case UPDATE_GOODS_INDEX:
                try {
                    List<String> goodsIds = JSONUtil.parseArray(new String(messageExt.getBody())).toList(String.class);
                    for (String goodsId : goodsIds) {
                        syncGoods(goodsId);
                    }
                } catch (Exception e) {
                }
                break;
            case GOODS_AUDIT:
                try {
                    Goods goods = JSONUtil.toBean(new String(messageExt.getBody()), Goods.class);
                    syncGoods(goods.getId());
                } catch (Exception e) {
                }
                break;
            case DOWN:
                try {
                    List<String> goodsIds = JSONUtil.parseArray(new String(messageExt.getBody())).toList(String.class);
                    for (String goodsId : goodsIds) {
                        updateStatus(goodsId, "PENDING");
                    }
                } catch (Exception e) {
                }
                break;
            case GOODS_DELETE:
                try {
                    List<String> goodsIds = JSONUtil.parseArray(new String(messageExt.getBody())).toList(String.class);
                    for (String goodsId : goodsIds) {
                        removeByGoodsId(goodsId);
                    }
                } catch (Exception e) {
                }
                break;
            default:
                break;
        }
    }

    private void syncGoods(String goodsId) {
        Goods goods = goodsService.getById(goodsId);
        if (goods == null) {
            return;
        }
        List<GoodsSku> skus = goodsSkuService.getGoodsSkuListByGoodsId(goodsId);
        if (CollUtil.isEmpty(skus)) {
            return;
        }
        String[] cids = CharSequenceUtil.splitToArray(goods.getCategoryPath(), ",");
        String lastCid = cids.length > 0 ? cids[cids.length - 1] : null;
        String categoryName = null;
        if (CharSequenceUtil.isNotEmpty(lastCid)) {
            Category c = categoryService.getById(lastCid);
            if (c != null) {
                categoryName = c.getName();
            }
        }
        for (GoodsSku sku : skus) {
            WxChannelGoods cg = findOrNew(goods.getId(), sku.getId());
            cg.setGoodsId(goods.getId());
            cg.setSkuId(sku.getId());
            cg.setGoodsName(CharSequenceUtil.isNotEmpty(sku.getSimpleSpecs()) ? goods.getGoodsName() + " " + sku.getSimpleSpecs() : goods.getGoodsName());
            cg.setGoodsImage(sku.getThumbnail());
            cg.setCategoryId(lastCid);
            cg.setCategoryName(categoryName);
            cg.setStoreId(goods.getStoreId());
            cg.setStoreName(goods.getStoreName());
            cg.setCostPrice(sku.getCost());
            cg.setChannelPrice(sku.getPrice());
            cg.setStock(sku.getQuantity());
            String status = statusFor(goods, sku);
            cg.setStatus(status);
            wxChannelGoodsService.saveOrUpdate(cg);
            wxChannelsApiService.updateInventory(sku.getId(), sku.getQuantity());
            wxChannelsApiService.changeStatus(goods.getId(), status);
        }
    }

    private WxChannelGoods findOrNew(String goodsId, String skuId) {
        WxChannelGoods exist = wxChannelGoodsService.getOne(new LambdaQueryWrapper<WxChannelGoods>()
                .eq(WxChannelGoods::getGoodsId, goodsId)
                .eq(WxChannelGoods::getSkuId, skuId)
                .last("limit 1"));
        return exist != null ? exist : new WxChannelGoods();
    }

    private String statusFor(Goods goods, GoodsSku sku) {
        boolean approved = CharSequenceUtil.equals(goods.getAuthFlag(), GoodsAuthEnum.PASS.name());
        boolean upper = CharSequenceUtil.equals(sku.getMarketEnable(), GoodsStatusEnum.UPPER.name());
        if (approved && upper) {
            return "APPROVED";
        }
        if (!approved) {
            return "REJECTED";
        }
        return "PENDING";
    }

    private void updateStatus(String goodsId, String status) {
        List<WxChannelGoods> list = wxChannelGoodsService.list(new LambdaQueryWrapper<WxChannelGoods>().eq(WxChannelGoods::getGoodsId, goodsId));
        for (WxChannelGoods g : list) {
            g.setStatus(status);
            wxChannelGoodsService.updateById(g);
            wxChannelsApiService.changeStatus(goodsId, status);
        }
    }

    private void removeByGoodsId(String goodsId) {
        List<WxChannelGoods> list = wxChannelGoodsService.list(new LambdaQueryWrapper<WxChannelGoods>().eq(WxChannelGoods::getGoodsId, goodsId));
        if (CollUtil.isNotEmpty(list)) {
            List<String> ids = list.stream().map(WxChannelGoods::getId).collect(Collectors.toList());
            wxChannelGoodsService.removeByIds(ids);
        }
        wxChannelsApiService.changeStatus(goodsId, "REJECTED");
    }
}
