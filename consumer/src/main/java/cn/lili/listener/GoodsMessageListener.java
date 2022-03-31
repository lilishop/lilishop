package cn.lili.listener;

import cn.hutool.core.map.MapUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ArrayUtil;
import cn.hutool.core.util.ClassLoaderUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.event.GoodsCommentCompleteEvent;
import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.dto.DistributionGoodsSearchParams;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.distribution.service.DistributionSelectedGoodsService;
import cn.lili.modules.goods.entity.dos.*;
import cn.lili.modules.goods.entity.dto.GoodsCompleteMessage;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.*;
import cn.lili.modules.member.entity.dos.FootPrint;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.member.service.FootprintService;
import cn.lili.modules.member.service.GoodsCollectionService;
import cn.lili.modules.promotion.entity.dos.BasePromotions;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.modules.store.service.StoreService;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 商品消息
 *
 * @author paulG
 * @since 2020/12/9
 **/
@Component
@Slf4j
@RocketMQMessageListener(topic = "${lili.data.rocketmq.goods-topic}", consumerGroup = "${lili.data.rocketmq.goods-group}")
public class GoodsMessageListener implements RocketMQListener<MessageExt> {

    /**
     * ES商品
     */
    @Autowired
    private EsGoodsIndexService goodsIndexService;
    /**
     * 店铺
     */
    @Autowired
    private StoreService storeService;
    /**
     * 商品
     */
    @Autowired
    private GoodsService goodsService;
    /**
     * 商品Sku
     */
    @Autowired
    private GoodsSkuService goodsSkuService;
    /**
     * 用户足迹
     */
    @Autowired
    private FootprintService footprintService;
    /**
     * 商品收藏
     */
    @Autowired
    private GoodsCollectionService goodsCollectionService;
    /**
     * 商品评价
     */
    @Autowired
    private List<GoodsCommentCompleteEvent> goodsCommentCompleteEvents;
    /**
     * 分销商品
     */
    @Autowired
    private DistributionGoodsService distributionGoodsService;
    /**
     * 分销员-商品关联表
     */
    @Autowired
    private DistributionSelectedGoodsService distributionSelectedGoodsService;
    /**
     * 分类
     */
    @Autowired
    private CategoryService categoryService;
    /**
     * 品牌
     */
    @Autowired
    private BrandService brandService;
    /**
     * 店铺商品分类
     */
    @Autowired
    private StoreGoodsLabelService storeGoodsLabelService;

    @Autowired
    private PromotionService promotionService;

    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Override
    public void onMessage(MessageExt messageExt) {

        switch (GoodsTagsEnum.valueOf(messageExt.getTags())) {
            //查看商品
            case VIEW_GOODS:
                FootPrint footPrint = JSONUtil.toBean(new String(messageExt.getBody()), FootPrint.class);
                footprintService.saveFootprint(footPrint);
                break;
            //生成索引
            case GENERATOR_GOODS_INDEX:
                try {
                    String goodsId = new String(messageExt.getBody());
                    log.info("生成索引: {}", goodsId);
                    Goods goods = this.goodsService.getById(goodsId);
                    updateGoodsIndex(goods);
                } catch (Exception e) {
                    log.error("生成商品索引事件执行异常，商品信息 {}", new String(messageExt.getBody()));
                }
                break;
            case UPDATE_GOODS_INDEX_PROMOTIONS:
                this.updateGoodsIndexPromotions(new String(messageExt.getBody()));
                break;
            case DELETE_GOODS_INDEX_PROMOTIONS:
                JSONObject jsonObject = JSONUtil.parseObj(new String(messageExt.getBody()));
                String promotionKey = jsonObject.getStr("promotionKey");
                if (CharSequenceUtil.isEmpty(promotionKey)) {
                    break;
                }
                if (CharSequenceUtil.isNotEmpty(jsonObject.getStr("scopeId"))) {
                    this.goodsIndexService.deleteEsGoodsPromotionByPromotionKey(Arrays.asList(jsonObject.getStr("scopeId").split(",")), promotionKey);
                } else {
                    this.goodsIndexService.deleteEsGoodsPromotionByPromotionKey(promotionKey);
                }
                break;
            case UPDATE_GOODS_INDEX:
                try {
                    String goodsIdsJsonStr = new String(messageExt.getBody());
                    GoodsSearchParams searchParams = new GoodsSearchParams();
                    searchParams.setId(ArrayUtil.join(JSONUtil.toList(goodsIdsJsonStr, String.class).toArray(), ","));
                    List<Goods> goodsList = goodsService.queryListByParams(searchParams);
                    this.updateGoodsIndex(goodsList);
                } catch (Exception e) {
                    log.error("更新商品索引事件执行异常，商品信息 {}", new String(messageExt.getBody()));
                }
                break;
            case UPDATE_GOODS_INDEX_FIELD:
                try {
                    String updateIndexFieldsJsonStr = new String(messageExt.getBody());
                    JSONObject updateIndexFields = JSONUtil.parseObj(updateIndexFieldsJsonStr);
                    @SuppressWarnings("unchecked")
                    Map<String, Object> queryFields = updateIndexFields.get("queryFields", Map.class);
                    @SuppressWarnings("unchecked")
                    Map<String, Object> updateFields = updateIndexFields.get("updateFields", Map.class);
                    goodsIndexService.updateIndex(queryFields, updateFields);
                } catch (Exception e) {
                    log.error("更新商品索引事件执行异常，商品信息 {}", new String(messageExt.getBody()));
                }
                break;
            case RESET_GOODS_INDEX:
                try {
                    String goodsIdsJsonStr = new String(messageExt.getBody());
                    List<EsGoodsIndex> goodsIndices = JSONUtil.toList(goodsIdsJsonStr, EsGoodsIndex.class);
                    goodsIndexService.updateBulkIndex(goodsIndices);
                } catch (Exception e) {
                    log.error("重置商品索引事件执行异常，商品信息 {}", new String(messageExt.getBody()));
                }
                break;
            //审核商品
            case GOODS_AUDIT:
                Goods goods = JSONUtil.toBean(new String(messageExt.getBody()), Goods.class);
                updateGoodsNum(goods);
                updateGoodsIndex(goods);
                break;
            //删除商品
            case GOODS_DELETE:
                try {
                    String goodsIdsJsonStr = new String(messageExt.getBody());
                    for (String goodsId : JSONUtil.toList(goodsIdsJsonStr, String.class)) {
                        Goods goodsById = this.goodsService.getById(goodsId);
                        if (goodsById != null) {
                            this.deleteGoods(goodsById);
                            this.updateGoodsNum(goodsById);
                            List<String> skuIdsByGoodsId = this.goodsSkuService.getSkuIdsByGoodsId(goodsId);
                            if (skuIdsByGoodsId != null && !skuIdsByGoodsId.isEmpty()) {
                                this.goodsIndexService.deleteIndexByIds(skuIdsByGoodsId);
                            }
                        }
                    }

                } catch (Exception e) {
                    log.error("删除商品索引事件执行异常，商品信息 {}", new String(messageExt.getBody()));
                }
                break;
            //规格删除
            case SKU_DELETE:
                String message = new String(messageExt.getBody());
                List<String> skuIds = JSONUtil.toList(message, String.class);
                goodsCollectionService.deleteSkuCollection(skuIds);
                break;
            //商品评价
            case GOODS_COMMENT_COMPLETE:
                MemberEvaluation memberEvaluation = JSONUtil.toBean(new String(messageExt.getBody()), MemberEvaluation.class);
                for (GoodsCommentCompleteEvent goodsCommentCompleteEvent : goodsCommentCompleteEvents) {
                    try {
                        goodsCommentCompleteEvent.goodsComment(memberEvaluation);
                    } catch (Exception e) {
                        log.error("评价{},在{}业务中，状态修改事件执行异常",
                                new String(messageExt.getBody()),
                                goodsCommentCompleteEvent.getClass().getName(),
                                e);
                    }
                }
                break;
            //购买商品完成
            case BUY_GOODS_COMPLETE:
                this.goodsBuyComplete(messageExt);
                break;
            default:
                log.error("商品执行异常：{}", new String(messageExt.getBody()));
                break;
        }
    }

    private void updateGoodsIndexPromotions(String promotionsJsonStr) {
        try {
            log.info("更新商品索引促销信息: {}", promotionsJsonStr);
            JSONObject jsonObject = JSONUtil.parseObj(promotionsJsonStr);
            BasePromotions promotions = (BasePromotions) jsonObject.get("promotions",
                    ClassLoaderUtil.loadClass(jsonObject.get("promotionsType").toString()));
            String esPromotionKey = jsonObject.get("esPromotionKey").toString();
            if (PromotionsScopeTypeEnum.PORTION_GOODS.name().equals(promotions.getScopeType())) {
                PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
                searchParams.setPromotionId(promotions.getId());
                List<PromotionGoods> promotionGoodsList = this.promotionGoodsService.listFindAll(searchParams);
                List<String> skuIds = promotionGoodsList.stream().map(PromotionGoods::getSkuId).collect(Collectors.toList());
                this.goodsIndexService.deleteEsGoodsPromotionByPromotionKey(skuIds, esPromotionKey);
                this.goodsIndexService.updateEsGoodsIndexByList(promotionGoodsList, promotions, esPromotionKey);
            } else if (PromotionsScopeTypeEnum.PORTION_GOODS_CATEGORY.name().equals(promotions.getScopeType())) {
                GoodsSearchParams searchParams = new GoodsSearchParams();
                searchParams.setCategoryPath(promotions.getScopeId());
                List<GoodsSku> goodsSkuByList = this.goodsSkuService.getGoodsSkuByList(searchParams);
                List<String> skuIds = goodsSkuByList.stream().map(GoodsSku::getId).collect(Collectors.toList());
                this.goodsIndexService.deleteEsGoodsPromotionByPromotionKey(skuIds, esPromotionKey);
                this.goodsIndexService.updateEsGoodsIndexPromotions(skuIds, promotions, esPromotionKey);
            } else if (PromotionsScopeTypeEnum.ALL.name().equals(promotions.getScopeType())) {
                this.goodsIndexService.deleteEsGoodsPromotionByPromotionKey(esPromotionKey);
                this.goodsIndexService.updateEsGoodsIndexAllByList(promotions, esPromotionKey);
            }
        } catch (Exception e) {
            log.error("生成商品索引促销信息执行异常", e);
        }
    }

    /**
     * 更新商品索引
     *
     * @param goodsList 商品列表消息
     */
    private void updateGoodsIndex(List<Goods> goodsList) {
        List<EsGoodsIndex> goodsIndices = new ArrayList<>();
        for (Goods goods : goodsList) {
            //如果商品通过审核&&并且已上架
            GoodsSearchParams searchParams = new GoodsSearchParams();
            searchParams.setGoodsId(goods.getId());
            searchParams.setGeQuantity(0);
            List<GoodsSku> goodsSkuList = this.goodsSkuService.getGoodsSkuByList(searchParams);
            if (goods.getAuthFlag().equals(GoodsAuthEnum.PASS.name())
                    && goods.getMarketEnable().equals(GoodsStatusEnum.UPPER.name())
                    && Boolean.FALSE.equals(goods.getDeleteFlag())) {
                goodsSkuList.forEach(goodsSku -> {
                    EsGoodsIndex goodsIndex = this.settingUpGoodsIndexData(goods, goodsSku);
                    goodsIndices.add(goodsIndex);
                });
            }
            //如果商品状态值不支持es搜索，那么将商品信息做下架处理
            else {
                for (GoodsSku goodsSku : goodsSkuList) {
                    EsGoodsIndex esGoodsOld = goodsIndexService.findById(goodsSku.getId());
                    if (esGoodsOld != null) {
                        goodsIndexService.deleteIndexById(goodsSku.getId());
                    }
                }
            }
        }
        goodsIndexService.updateBulkIndex(goodsIndices);
    }

    /**
     * 更新商品索引
     *
     * @param goods 商品消息
     */
    private void updateGoodsIndex(Goods goods) {
        //如果商品通过审核&&并且已上架
        GoodsSearchParams searchParams = new GoodsSearchParams();
        searchParams.setGoodsId(goods.getId());
        List<GoodsSku> goodsSkuList = this.goodsSkuService.getGoodsSkuByList(searchParams);
        log.info("goods：{}", goods);
        log.info("goodsSkuList：{}", goodsSkuList);
        if (goods.getAuthFlag().equals(GoodsAuthEnum.PASS.name())
                && goods.getMarketEnable().equals(GoodsStatusEnum.UPPER.name())
                && Boolean.FALSE.equals(goods.getDeleteFlag())) {
            this.generatorGoodsIndex(goods, goodsSkuList);
        }
        //如果商品状态值不支持es搜索，那么将商品信息做下架处理
        else {
            for (GoodsSku goodsSku : goodsSkuList) {
                EsGoodsIndex esGoodsOld = goodsIndexService.findById(goodsSku.getId());
                if (esGoodsOld != null) {
                    goodsIndexService.deleteIndexById(goodsSku.getId());
                }
            }
        }
    }

    /**
     * 生成商品索引
     *
     * @param goods        商品信息
     * @param goodsSkuList 商品sku信息
     */
    private void generatorGoodsIndex(Goods goods, List<GoodsSku> goodsSkuList) {
        int skuSource = 100;
        for (GoodsSku goodsSku : goodsSkuList) {
            EsGoodsIndex esGoodsOld = goodsIndexService.findById(goodsSku.getId());
            EsGoodsIndex goodsIndex = this.settingUpGoodsIndexData(goods, goodsSku);
            goodsIndex.setSkuSource(skuSource--);
            log.info("goodsSku：{}", goodsSku);
            log.info("esGoodsOld：{}", esGoodsOld);
            //如果商品库存不为0，并且es中有数据
            if (goodsSku.getQuantity() > 0 && esGoodsOld == null) {
                log.info("生成商品索引 {}", goodsIndex);
                this.goodsIndexService.addIndex(goodsIndex);
            } else if (goodsSku.getQuantity() > 0 && esGoodsOld != null) {
                goodsIndexService.updateIndex(goodsIndex);
            }
        }
    }

    private EsGoodsIndex settingUpGoodsIndexData(Goods goods, GoodsSku goodsSku) {
        EsGoodsIndex goodsIndex = new EsGoodsIndex(goodsSku);
        if (goods.getParams() != null && !goods.getParams().isEmpty()) {
            List<GoodsParamsDTO> goodsParamDTOS = JSONUtil.toList(goods.getParams(), GoodsParamsDTO.class);
            goodsIndex = new EsGoodsIndex(goodsSku, goodsParamDTOS);
        }
        goodsIndex.setAuthFlag(goods.getAuthFlag());
        goodsIndex.setMarketEnable(goods.getMarketEnable());
        this.settingUpGoodsIndexOtherParam(goodsIndex);
        return goodsIndex;
    }

    /**
     * 设置商品索引的其他参数（非商品自带）
     *
     * @param goodsIndex 商品索引信息
     */
    private void settingUpGoodsIndexOtherParam(EsGoodsIndex goodsIndex) {
        List<Category> categories = categoryService.listByIdsOrderByLevel(Arrays.asList(goodsIndex.getCategoryPath().split(",")));
        if (!categories.isEmpty()) {
            goodsIndex.setCategoryNamePath(ArrayUtil.join(categories.stream().map(Category::getName).toArray(), ","));
        }
        Brand brand = brandService.getById(goodsIndex.getBrandId());
        if (brand != null) {
            goodsIndex.setBrandName(brand.getName());
            goodsIndex.setBrandUrl(brand.getLogo());
        }
        if (goodsIndex.getStoreCategoryPath() != null && CharSequenceUtil.isNotEmpty(goodsIndex.getStoreCategoryPath())) {
            List<StoreGoodsLabel> storeGoodsLabels = storeGoodsLabelService.listByStoreIds(Arrays.asList(goodsIndex.getStoreCategoryPath().split(",")));
            if (!storeGoodsLabels.isEmpty()) {
                goodsIndex.setStoreCategoryNamePath(ArrayUtil.join(storeGoodsLabels.stream().map(StoreGoodsLabel::getLabelName).toArray(), ","));
            }
        }

        if (goodsIndex.getOriginPromotionMap() == null || goodsIndex.getOriginPromotionMap().isEmpty()) {
            Map<String, Object> goodsCurrentPromotionMap = promotionService.getGoodsSkuPromotionMap(goodsIndex.getStoreId(), goodsIndex.getId());
            goodsIndex.setPromotionMapJson(JSONUtil.toJsonStr(goodsCurrentPromotionMap));
        }
    }


    /**
     * 删除商品
     * 1.更新店铺的商品数量
     * 2.删除分销员-分销商品绑定关系
     * 3.删除分销商品
     *
     * @param goods 消息
     */
    private void deleteGoods(Goods goods) {

        DistributionGoodsSearchParams searchParams = new DistributionGoodsSearchParams();
        searchParams.setGoodsId(goods.getId());
        //删除获取分销商品
        DistributionGoods distributionGoods = distributionGoodsService.getDistributionGoods(searchParams);

        if (distributionGoods != null) {

            //删除分销商品绑定关系
            distributionSelectedGoodsService.deleteByDistributionGoodsId(distributionGoods.getId());

            //删除分销商品
            distributionGoodsService.removeById(distributionGoods.getId());
        }
    }

    /**
     * 修改商品数量
     *
     * @param goods 信息体
     */
    private void updateGoodsNum(Goods goods) {
        try {
            //更新店铺商品数量
            assert goods != null;
            storeService.updateStoreGoodsNum(goods.getStoreId());
        } catch (Exception e) {
            log.error("修改商品数量错误");
        }
    }

    /**
     * 商品购买完成
     * 1.更新商品购买数量
     * 2.更新SKU购买数量
     * 3.更新索引购买数量
     *
     * @param messageExt 信息体
     */
    private void goodsBuyComplete(MessageExt messageExt) {
        String goodsCompleteMessageStr = new String(messageExt.getBody());
        List<GoodsCompleteMessage> goodsCompleteMessageList = JSONUtil.toList(JSONUtil.parseArray(goodsCompleteMessageStr), GoodsCompleteMessage.class);
        for (GoodsCompleteMessage goodsCompleteMessage : goodsCompleteMessageList) {
            Goods goods = goodsService.getById(goodsCompleteMessage.getGoodsId());
            if (goods != null) {
                //更新商品购买数量
                if (goods.getBuyCount() == null) {
                    goods.setBuyCount(0);
                }
                int buyCount = goods.getBuyCount() + goodsCompleteMessage.getBuyNum();
                this.goodsService.updateGoodsBuyCount(goodsCompleteMessage.getGoodsId(), buyCount);
            } else {
                log.error("商品Id为[" + goodsCompleteMessage.getGoodsId() + "的商品不存在，更新商品失败！");
            }
            GoodsSku goodsSku = goodsSkuService.getById(goodsCompleteMessage.getSkuId());
            if (goodsSku != null) {
                //更新商品购买数量
                if (goodsSku.getBuyCount() == null) {
                    goodsSku.setBuyCount(0);
                }
                int buyCount = goodsSku.getBuyCount() + goodsCompleteMessage.getBuyNum();
                goodsSku.setBuyCount(buyCount);
                goodsSkuService.update(goodsSku);

                this.goodsIndexService.updateIndex(
                        MapUtil.builder(new HashMap<String, Object>()).put("id", goodsCompleteMessage.getSkuId()).build(),
                        MapUtil.builder(new HashMap<String, Object>()).put("buyCount", buyCount).build());

            } else {
                log.error("商品SkuId为[" + goodsCompleteMessage.getGoodsId() + "的商品不存在，更新商品失败！");
            }
        }
    }
}
