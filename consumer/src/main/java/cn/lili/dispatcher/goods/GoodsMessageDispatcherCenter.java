package cn.lili.dispatcher.goods;

import cn.hutool.core.map.MapUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ArrayUtil;
import cn.hutool.core.util.ClassLoaderUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.annotation.GoodsMQTagRoute;
import cn.lili.common.exception.RetryException;
import cn.lili.common.vo.PageVO;
import cn.lili.event.GoodsCommentCompleteEvent;
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
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

import static cn.lili.rocketmq.tags.GoodsTagsEnum.*;


//基于方法的路由测试
@Slf4j
@RequiredArgsConstructor
@Component
public class GoodsMessageDispatcherCenter {
    private static final int BATCH_SIZE = 10;
    /**
     * ES商品
     */
    private final EsGoodsIndexService goodsIndexService;
    /**
     * 商品
     */
    private final GoodsService goodsService;
    /**
     * 商品Sku
     */
    private final GoodsSkuService goodsSkuService;
    /**
     * 用户足迹
     */
    private final FootprintService footprintService;
    /**
     * 商品收藏
     */
    private final GoodsCollectionService goodsCollectionService;
    /**
     * 商品评价
     */
    private final List<GoodsCommentCompleteEvent> goodsCommentCompleteEvents;
    /**
     * 分销商品
     */
    private final DistributionGoodsService distributionGoodsService;
    /**
     * 分销员-商品关联表
     */
    private final DistributionSelectedGoodsService distributionSelectedGoodsService;
    /**
     * 分类
     */
    private final CategoryService categoryService;
    /**
     * 品牌
     */
    private final BrandService brandService;
    /**
     * 店铺商品分类
     */
    private final StoreGoodsLabelService storeGoodsLabelService;

    private final PromotionService promotionService;

    private final PromotionGoodsService promotionGoodsService;

    @GoodsMQTagRoute(GoodsTagsEnum.VIEW_GOODS)
    public void viewGoods(MessageExt messageExt) {
        FootPrint footPrint = JSONUtil.toBean(new String(messageExt.getBody()), FootPrint.class);
        footprintService.saveFootprint(footPrint);
    }

    @GoodsMQTagRoute(GoodsTagsEnum.GENERATOR_GOODS_INDEX)
    public void createGoodsIndex(MessageExt messageExt) {
        try {
            String goodsId = new String(messageExt.getBody());
            log.info("生成索引: {}", goodsId);
            Goods goods = this.goodsService.getById(goodsId);
            this.updateGoodsIndex(goods);
        } catch (Exception e) {
            log.error("生成商品索引事件执行异常，商品信息: " + new String(messageExt.getBody()), e);
        }
    }

    @GoodsMQTagRoute(GoodsTagsEnum.GENERATOR_STORE_GOODS_INDEX)
    public void createStoreIndex(MessageExt messageExt) {
        try {
            String storeId = new String(messageExt.getBody());
            this.updateGoodsIndex(storeId);
        } catch (Exception e) {
            log.error("生成店铺商品索引事件执行异常，商品信息: " + new String(messageExt.getBody()), e);
        }
    }

    @GoodsMQTagRoute(GoodsTagsEnum.UPDATE_GOODS_INDEX_PROMOTIONS)
    public void updateGoodsIndexPromo(MessageExt messageExt) {
        this.updateGoodsIndexPromotions(new String(messageExt.getBody()));
    }

    @GoodsMQTagRoute(GoodsTagsEnum.DELETE_GOODS_INDEX_PROMOTIONS)
    public void deleteGoodsIndexPromotions(MessageExt messageExt) {
        JSONObject jsonObject = JSONUtil.parseObj(new String(messageExt.getBody()));
        String promotionKey = jsonObject.getStr("promotionKey");
        if (CharSequenceUtil.isNotEmpty(jsonObject.getStr("scopeId"))) {
            this.goodsIndexService.deleteEsGoodsPromotionByPromotionKey(Arrays.asList(jsonObject.getStr("scopeId").split(",")), promotionKey);
        } else {
            this.goodsIndexService.deleteEsGoodsPromotionByPromotionKey(promotionKey);
        }
    }

    @GoodsMQTagRoute(UPDATE_GOODS_INDEX)
    public void updateBatchGoodsIndex(MessageExt messageExt) {
        try {
            String goodsIdsJsonStr = new String(messageExt.getBody());
            GoodsSearchParams searchParams = new GoodsSearchParams();
            searchParams.setId(ArrayUtil.join(JSONUtil.toList(goodsIdsJsonStr, String.class).toArray(), ","));
            List<Goods> goodsList = goodsService.queryListByParams(searchParams);
            this.updateGoodsIndex(goodsList);
        } catch (Exception e) {
            log.error("更新商品索引事件执行异常，商品信息: " + new String(messageExt.getBody()), e);
        }
    }

    @GoodsMQTagRoute(UPDATE_GOODS_INDEX_FIELD)
    public void updateGoodsIndexField(MessageExt messageExt) {
        try {
            String updateIndexFieldsJsonStr = new String(messageExt.getBody());
            JSONObject updateIndexFields = JSONUtil.parseObj(updateIndexFieldsJsonStr);
            @SuppressWarnings("unchecked") Map<String, Object> queryFields = updateIndexFields.get("queryFields", Map.class);
            @SuppressWarnings("unchecked") Map<String, Object> updateFields = updateIndexFields.get("updateFields", Map.class);
            goodsIndexService.updateIndex(queryFields, updateFields);
        } catch (Exception e) {
            log.error("更新商品索引事件执行异常，商品信息: " + new String(messageExt.getBody()), e);
        }
    }

    @GoodsMQTagRoute(RESET_GOODS_INDEX)
    public void resetGoodsIndex(MessageExt messageExt) {
        try {
            String goodsIdsJsonStr = new String(messageExt.getBody());
            List<EsGoodsIndex> goodsIndices = JSONUtil.toList(goodsIdsJsonStr, EsGoodsIndex.class);
            goodsIndexService.updateBulkIndex(goodsIndices);
        } catch (Exception e) {
            log.error("重置商品索引事件执行异常，商品信息: " + new String(messageExt.getBody()), e);
        }
    }

    @GoodsMQTagRoute(GOODS_AUDIT)
    public void goodsAudit(MessageExt messageExt) {
        Goods goods = JSONUtil.toBean(new String(messageExt.getBody()), Goods.class);
        updateGoodsIndex(goods);
    }

    @GoodsMQTagRoute(GOODS_DELETE)
    public void goodsDelete(MessageExt messageExt) {
        try {
            String goodsIdsJsonStr = new String(messageExt.getBody());
            for (String goodsId : JSONUtil.toList(goodsIdsJsonStr, String.class)) {
                goodsIndexService.deleteIndex(MapUtil.builder(new HashMap<String, Object>()).put("goodsId", goodsId).build());
            }

            promotionService.removeByGoodsIds(goodsIdsJsonStr);
        } catch (Exception e) {
            log.error("删除商品索引事件执行异常，商品信息: " + new String(messageExt.getBody()), e);
        }
    }

    @GoodsMQTagRoute(DOWN)
    public void removeGoods(MessageExt messageExt) {
        String goodsIdsJsonStr = new String(messageExt.getBody());
        promotionService.removeByGoodsIds(goodsIdsJsonStr);
    }


    @GoodsMQTagRoute(SKU_DELETE)
    public void skuDelete(MessageExt messageExt) {
        String message = new String(messageExt.getBody());
        List<String> skuIds = JSONUtil.toList(message, String.class);
        goodsCollectionService.deleteSkuCollection(skuIds);
    }

    @GoodsMQTagRoute(STORE_GOODS_DELETE)
    public void storeGoodsDelete(MessageExt messageExt) {
        try {
            String storeId = new String(messageExt.getBody());
            goodsIndexService.deleteIndex(MapUtil.builder(new HashMap<String, Object>()).put("storeId", storeId).build());
        } catch (RetryException re) {
            throw re;
        } catch (Exception e) {
            log.error("删除店铺商品索引事件执行异常，商品信息: " + new String(messageExt.getBody()), e);
        }
    }

    ////同步商品分类名称
    @GoodsMQTagRoute(CATEGORY_GOODS_NAME)
    public void categoryGoodsName(MessageExt messageExt) {
        //分类ID
        String id = new String(messageExt.getBody());
        goodsService.categoryGoodsName(id);
    }

    //商品评价
    @GoodsMQTagRoute(GOODS_COMMENT_COMPLETE)
    public void goodsCommentComplete(MessageExt messageExt) {
        MemberEvaluation memberEvaluation = JSONUtil.toBean(new String(messageExt.getBody()), MemberEvaluation.class);
        for (GoodsCommentCompleteEvent goodsCommentCompleteEvent : goodsCommentCompleteEvents) {
            try {
                goodsCommentCompleteEvent.goodsComment(memberEvaluation);
            } catch (Exception e) {
                log.error("评价{},在{}业务中，状态修改事件执行异常", new String(messageExt.getBody()), goodsCommentCompleteEvent.getClass().getName(), e);
            }
        }
    }

    @GoodsMQTagRoute(BUY_GOODS_COMPLETE)
    public void buyGoodsComplete(MessageExt messageExt) {
        this.goodsBuyComplete(messageExt);
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
                goodsSkuService.updateGoodsSkuBuyCount(goodsSku.getId(), buyCount);

                this.goodsIndexService.updateIndex(MapUtil.builder(new HashMap<String, Object>()).put("id", goodsCompleteMessage.getSkuId()).build(), MapUtil.builder(new HashMap<String, Object>()).put("buyCount", buyCount).build());

            } else {
                log.error("商品SkuId为[" + goodsCompleteMessage.getGoodsId() + "的商品不存在，更新商品失败！");
            }
        }
    }

    private void updateGoodsIndexPromotions(String promotionsJsonStr) {
        try {
            log.info("更新商品索引促销信息: {}", promotionsJsonStr);
            JSONObject jsonObject = JSONUtil.parseObj(promotionsJsonStr);
            // 转换为详细的促销信息（注：促销信息必须继承自 BasePromotions，且必须保证派生类存在与sdk包下）
            BasePromotions promotions = (BasePromotions) jsonObject.get("promotions", ClassLoaderUtil.loadClass(jsonObject.get("promotionsType").toString()));
            // 获取促销唯一key,由 促销类型 + 促销id 组成
            String esPromotionKey = jsonObject.get("esPromotionKey").toString();
            if (PromotionsScopeTypeEnum.PORTION_GOODS.name().equals(promotions.getScopeType())) {
                for (int i = 0; ; i++) {
                    PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
                    searchParams.setPromotionId(promotions.getId());
                    PageVO pageVO = new PageVO();
                    pageVO.setPageNumber(i);
                    pageVO.setPageSize(BATCH_SIZE);
                    Page<PromotionGoods> promotionGoodsPage = this.promotionGoodsService.pageFindAll(searchParams, pageVO);
                    if (promotionGoodsPage == null || promotionGoodsPage.getRecords().isEmpty()) {
                        break;
                    }
                    List<String> skuIds = promotionGoodsPage.getRecords().stream().map(PromotionGoods::getSkuId).collect(Collectors.toList());
                    // 更新商品索引促销信息（删除原索引中相关的促销信息，更新索引中促销信息）
                    this.goodsIndexService.deleteEsGoodsPromotionByPromotionKey(skuIds, esPromotionKey);
                    this.goodsIndexService.updateEsGoodsIndexByList(promotionGoodsPage.getRecords(), promotions, esPromotionKey);
                }

            } else if (PromotionsScopeTypeEnum.PORTION_GOODS_CATEGORY.name().equals(promotions.getScopeType())) {
                for (int i = 0; ; i++) {
                    GoodsSearchParams searchParams = new GoodsSearchParams();
                    searchParams.setCategoryPath(promotions.getScopeId());
                    searchParams.setPageNumber(i);
                    searchParams.setPageSize(BATCH_SIZE);
                    if (CharSequenceUtil.isNotEmpty(promotions.getStoreId()) && !"0".equals(promotions.getStoreId())) {
                        searchParams.setStoreId(promotions.getStoreId());
                    }
                    IPage<GoodsSku> goodsSkuByPage = this.goodsSkuService.getGoodsSkuByPage(searchParams);
                    if (goodsSkuByPage == null || goodsSkuByPage.getRecords().isEmpty()) {
                        break;
                    }
                    List<String> skuIds = goodsSkuByPage.getRecords().stream().map(GoodsSku::getId).collect(Collectors.toList());
                    // 更新商品索引促销信息（删除原索引中相关的促销信息，更新索引中促销信息）
                    this.goodsIndexService.deleteEsGoodsPromotionByPromotionKey(skuIds, esPromotionKey);
                    this.goodsIndexService.updateEsGoodsIndexPromotions(skuIds, promotions, esPromotionKey);
                }

            } else if (PromotionsScopeTypeEnum.ALL.name().equals(promotions.getScopeType())) {
                this.goodsIndexService.updateEsGoodsIndexAllByList(promotions, esPromotionKey);
            }
        } catch (Exception e) {
            log.error("生成商品索引促销信息执行异常", e);
        }
    }

    /**
     * 更新商品索引根据店铺id
     *
     * @param storeId 店铺id
     */
    private void updateGoodsIndex(String storeId) {
        //如果商品通过审核&&并且已上架
        GoodsSearchParams searchParams = new GoodsSearchParams();
        searchParams.setStoreId(storeId);
        for (Goods goods : this.goodsService.queryListByParams(searchParams)) {
            this.updateGoodsIndex(goods);
        }

    }

    /**
     * 更新商品索引
     *
     * @param goodsList 商品列表消息
     */
    private void updateGoodsIndex(List<Goods> goodsList) {
        for (Goods goods : goodsList) {
            this.updateGoodsIndex(goods);
        }
    }

    /**
     * 更新商品索引
     *
     * @param goods 商品消息
     */
    private void updateGoodsIndex(Goods goods) {
        for (int i = 1; ; i++) {
            //如果商品通过审核&&并且已上架
            GoodsSearchParams searchParams = new GoodsSearchParams();
            searchParams.setGoodsId(goods.getId());
            searchParams.setPageNumber(i);
            searchParams.setPageSize(BATCH_SIZE);
            searchParams.setGeQuantity(0);
            IPage<GoodsSku> goodsSkuByPage = this.goodsSkuService.getGoodsSkuByPage(searchParams);
            if (goodsSkuByPage == null || goodsSkuByPage.getRecords().isEmpty()) {
                break;
            }
            log.info("goods：{}", goods);
            log.info("goodsSkuList：{}", goodsSkuByPage.getRecords());
            if (goods.getAuthFlag().equals(GoodsAuthEnum.PASS.name()) && goods.getMarketEnable().equals(GoodsStatusEnum.UPPER.name()) && Boolean.FALSE.equals(goods.getDeleteFlag())) {
                this.generatorGoodsIndex(goods, goodsSkuByPage.getRecords());
            } else {
                //如果商品状态值不支持es搜索，那么将商品信息做下架处理
                for (GoodsSku goodsSku : goodsSkuByPage.getRecords()) {
                    EsGoodsIndex esGoodsOld = goodsIndexService.findById(goodsSku.getId());
                    if (esGoodsOld != null) {
                        goodsIndexService.deleteIndexById(goodsSku.getId());
                    }
                }
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
     * 生成商品索引
     *
     * @param goods        商品信息
     * @param goodsSkuList 商品sku信息
     */
    private void generatorGoodsIndex(Goods goods, List<GoodsSku> goodsSkuList) {
        int skuSource = 100;
        List<EsGoodsIndex> esGoodsIndices = new ArrayList<>();
        for (GoodsSku goodsSku : goodsSkuList) {
            EsGoodsIndex goodsIndex = this.settingUpGoodsIndexData(goods, goodsSku);
            skuSource--;
            if (skuSource <= 0) {
                skuSource = 1;
            }
            goodsIndex.setSkuSource(skuSource);
            log.info("goodsSku：{}", goodsSku);
            log.info("生成商品索引 {}", goodsIndex);
            esGoodsIndices.add(goodsIndex);
        }
        this.goodsIndexService.deleteIndex(MapUtil.builder(new HashMap<String, Object>()).put("goodsId", goods.getId()).build());
        this.goodsIndexService.addIndex(esGoodsIndices);
    }
}
