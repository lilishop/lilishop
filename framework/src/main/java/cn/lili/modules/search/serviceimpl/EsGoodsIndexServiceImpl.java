package cn.lili.modules.search.serviceimpl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.extra.pinyin.PinyinUtil;
import cn.lili.common.elasticsearch.BaseElasticsearchService;
import cn.lili.common.elasticsearch.EsSuffix;
import cn.lili.config.elasticsearch.ElasticsearchProperties;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dos.GoodsWords;
import cn.lili.modules.goods.entity.enums.GoodsWordsTypeEnum;
import cn.lili.modules.goods.service.GoodsWordsService;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.BasePromotion;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.modules.search.entity.dos.EsGoodsAttribute;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import cn.lili.modules.search.repository.EsGoodsIndexRepository;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.modules.search.service.EsGoodsSearchService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.assertj.core.util.IterableUtil;
import org.elasticsearch.action.search.SearchResponse;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.indices.AnalyzeRequest;
import org.elasticsearch.client.indices.AnalyzeResponse;
import org.elasticsearch.search.SearchHit;
import org.mybatis.spring.MyBatisSystemException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 商品索引业务层实现
 * @author paulG
 * @since 2020/10/14
 **/
@Slf4j
@Service
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class EsGoodsIndexServiceImpl extends BaseElasticsearchService implements EsGoodsIndexService {

    private final ElasticsearchProperties elasticsearchProperties;
    private final EsGoodsIndexRepository goodsIndexRepository;
    private final EsGoodsSearchService goodsSearchService;
    private final GoodsWordsService goodsWordsService;

    private PromotionService promotionService;

    @Autowired
    public void setPromotionService(PromotionService promotionService) {
        this.promotionService = promotionService;
    }

    @Override
    public void addIndex(EsGoodsIndex goods) {
        String indexName = elasticsearchProperties.getIndexPrefix() + "_" + EsSuffix.GOODS_INDEX_NAME;
        try {
            AnalyzeRequest analyzeRequest = AnalyzeRequest.withIndexAnalyzer(indexName, "ik_max_word", goods.getGoodsName());
            AnalyzeResponse analyze = client.indices().analyze(analyzeRequest, RequestOptions.DEFAULT);
            List<AnalyzeResponse.AnalyzeToken> tokens = analyze.getTokens();
            if (goods.getAttrList() != null && !goods.getAttrList().isEmpty()) {
                for (EsGoodsAttribute esGoodsAttribute : goods.getAttrList()) {
                    wordsToDb(esGoodsAttribute.getValue());
                }
            }
            for (AnalyzeResponse.AnalyzeToken token : tokens) {
                wordsToDb(token.getTerm());
            }
            goodsIndexRepository.save(goods);
        } catch (IOException e) {
            log.error("为商品[" + goods.getGoodsName() + "]生成索引异常", e);
        }
    }

    @Override
    public void updateIndex(EsGoodsIndex goods) {
        goodsIndexRepository.save(goods);
    }

    /**
     * 更新商品索引的购买数量
     *
     * @param id       商品索引id
     * @param buyCount 更新后的购买数量
     */
    @Override
    public void updateIndexBuyNum(String id, Integer buyCount) {
        EsGoodsIndex goodsIndex = this.findById(id);
        goodsIndex.setBuyCount(buyCount);
        this.updateIndex(goodsIndex);
    }

    /**
     * 更新商品索引的评论相关数据
     *
     * @param id            商品索引ID
     * @param commentNum    评论数量
     * @param highPraiseNum 好评数量
     * @param grade         好评率
     */
    @Override
    public void updateIndexCommentNum(String id, Integer commentNum, Integer highPraiseNum, Double grade) {
        EsGoodsIndex goodsIndex = this.findById(id);
        goodsIndex.setCommentNum(commentNum);
        goodsIndex.setHighPraiseNum(highPraiseNum);
        goodsIndex.setGrade(grade);
        this.updateIndex(goodsIndex);
    }

    @Override
    public void deleteIndex(EsGoodsIndex goods) {
        if (ObjectUtils.isEmpty(goods)) {
            // 如果对象为空，则删除全量
            goodsIndexRepository.deleteAll();
        }
        goodsIndexRepository.delete(goods);
    }

    /**
     * 删除索引
     *
     * @param id 商品索引信息
     */
    @Override
    public void deleteIndexById(String id) {
        goodsIndexRepository.deleteById(id);
    }

    @Override
    public void initIndex(List<EsGoodsIndex> goodsIndexList) {
        String indexName = elasticsearchProperties.getIndexPrefix() + "_" + EsSuffix.GOODS_INDEX_NAME;
        // deleteIndexRequest(indexName);
        if (!indexExist(indexName)) {
            createIndexRequest(indexName);
        }
        if (goodsIndexList != null && !goodsIndexList.isEmpty()) {
            goodsIndexRepository.deleteAll();
            for (EsGoodsIndex goodsIndex : goodsIndexList) {
                addIndex(goodsIndex);
            }
        }
    }

    @Override
    public void updateEsGoodsIndex(String id, BasePromotion promotion, String key, Double price) {
        EsGoodsIndex goodsIndex = findById(id);
        if (goodsIndex != null) {
            if (promotion.getPromotionStatus().equals(PromotionStatusEnum.START.name()) && price != null) {
                goodsIndex.setPromotionPrice(price);
            } else {
                goodsIndex.setPromotionPrice(goodsIndex.getPrice());
            }
            this.updateGoodsIndexPromotion(goodsIndex, key, promotion);
        } else {
            log.error("更新索引商品促销信息失败！skuId 为 【{}】的索引不存在！", id);
        }
    }

    @Override
    public void updateEsGoodsIndexByList(List<PromotionGoods> promotionGoodsList, BasePromotion promotion, String key) {
        if (promotionGoodsList != null) {
            for (PromotionGoods promotionGoods : promotionGoodsList) {
                updateEsGoodsIndex(promotionGoods.getSkuId(), promotion, key, promotionGoods.getPrice());
            }
        }

    }

    /**
     * 更新全部商品索引的促销信息
     *
     * @param promotion 促销信息
     * @param key       促销信息的key
     */
    @Override
    public void updateEsGoodsIndexAllByList(BasePromotion promotion, String key) {
        List<EsGoodsIndex> goodsIndices;
        if (promotion.getStoreId() != null) {
            EsGoodsSearchDTO searchDTO = new EsGoodsSearchDTO();
            searchDTO.setStoreId(promotion.getStoreId());
            Page<EsGoodsIndex> esGoodsIndices = goodsSearchService.searchGoods(searchDTO, null);
            goodsIndices = esGoodsIndices.getContent();
        } else {
            Iterable<EsGoodsIndex> all = goodsIndexRepository.findAll();
            goodsIndices = new ArrayList<>(IterableUtil.toCollection(all));
        }
        for (EsGoodsIndex goodsIndex : goodsIndices) {
            this.updateGoodsIndexPromotion(goodsIndex, key, promotion);
        }
    }

    @Override
    public void deleteEsGoodsPromotionIndexByList(List<String> skuIds, PromotionTypeEnum promotionType) {
        for (String skuId : skuIds) {
            EsGoodsIndex goodsIndex = findById(skuId);
            if (goodsIndex != null) {
                Map<String, Object> promotionMap = goodsIndex.getPromotionMap();
                if (promotionMap != null && !promotionMap.isEmpty()) {
                    // 如果存在同类型促销活动删除
                    List<String> collect = promotionMap.keySet().parallelStream().filter(i -> i.contains(promotionType.name())).collect(Collectors.toList());
                    collect.forEach(promotionMap::remove);
                    goodsIndex.setPromotionMap(promotionMap);
                    updateIndex(goodsIndex);
                }
            } else {
                log.error("更新索引商品促销信息失败！skuId 为 【{}】的索引不存在！", skuId);
            }
        }
    }

    /**
     * 清除所以商品索引的无效促销活动
     */
    @Override
    public void cleanInvalidPromotion() {
        Iterable<EsGoodsIndex> all = goodsIndexRepository.findAll();
        for (EsGoodsIndex goodsIndex : all) {
            Map<String, Object> promotionMap = goodsIndex.getPromotionMap();
            if (promotionMap != null && !promotionMap.isEmpty()) {
                for (Map.Entry<String, Object> entry : promotionMap.entrySet()) {
                    BasePromotion promotion = (BasePromotion) entry.getValue();
                    if (promotion.getEndTime().getTime() > DateUtil.date().getTime()) {
                        promotionMap.remove(entry.getKey());
                    }
                }
            }
        }
        goodsIndexRepository.saveAll(all);
    }

    @Override
    public EsGoodsIndex findById(String id) {
        Optional<EsGoodsIndex> goodsIndex = goodsIndexRepository.findById(id);
        if (!goodsIndex.isPresent()) {
            log.error("商品skuId为" + id + "的es索引不存在！");
            return null;
        }
        return goodsIndex.get();
    }

    /**
     * 根据id获取商品索引信息的促销信息
     *
     * @param id skuId
     * @return 促销信息map
     */
    @Override
    public Map<String, Object> getPromotionMap(String id) {
        EsGoodsIndex goodsIndex = this.findById(id);
        if (goodsIndex != null) {
            Map<String, Object> promotionMap = goodsIndex.getPromotionMap();
            if (promotionMap == null || promotionMap.isEmpty()) {
                return new HashMap<>();
            }
            return promotionMap;
        }
        return null;
    }

    /**
     * 根据id获取商品索引信息的指定促销活动的id
     *
     * @param id                skuId
     * @param promotionTypeEnum 促销活动类型
     * @return 当前商品参与的促销活动id集合
     */
    @Override
    public List<String> getPromotionIdByPromotionType(String id, PromotionTypeEnum promotionTypeEnum) {
        Map<String, Object> promotionMap = this.getPromotionMap(id);
        if (promotionMap == null || promotionMap.isEmpty()) {
            return new ArrayList<>();
        }
        List<String> keyCollect = promotionMap.keySet().stream().filter(i -> i.contains(promotionTypeEnum.name())).collect(Collectors.toList());
        List<String> promotionIds = new ArrayList<>();
        for (String key : keyCollect) {
            BasePromotion promotion = (BasePromotion) promotionMap.get(key);
            promotionIds.add(promotion.getId());
        }
        return promotionIds;
    }

    /**
     * 重置当前商品索引
     *
     * @param goodsSku 商品sku信息
     * @return 商品索引
     */
    @Override
    public EsGoodsIndex resetEsGoodsIndex(GoodsSku goodsSku) {
        EsGoodsIndex index = new EsGoodsIndex(goodsSku);
        Map<String, Object> goodsCurrentPromotionMap = promotionService.getGoodsCurrentPromotionMap(index);
        index.setPromotionMap(goodsCurrentPromotionMap);
        this.addIndex(index);
        return index;
    }

    private void updateGoodsIndexPromotion(EsGoodsIndex goodsIndex, String key, BasePromotion promotion) {
        Map<String, Object> promotionMap;
        if (goodsIndex.getPromotionMap() == null || goodsIndex.getPromotionMap().isEmpty()) {
            promotionMap = new HashMap<>(1);
        } else {
            promotionMap = goodsIndex.getPromotionMap();
        }


        if (promotion.getPromotionStatus().equals(PromotionStatusEnum.END.name()) || promotion.getPromotionStatus().equals(PromotionStatusEnum.CLOSE.name())) {
            if (promotionMap.containsKey(key)) {
                promotionMap.remove(key);
            } else {
                this.removePromotionKey(key, promotionMap, PromotionTypeEnum.SECKILL.name());
            }
        } else {
            // 添加促销活动前，如果是同一时间只可以有一个的活动，但商品索引的促销活动里存在其他（同一时间只可以有一个）的活动，则清除
            this.removePromotionKey(key, promotionMap, PromotionTypeEnum.PINTUAN.name(), PromotionTypeEnum.SECKILL.name(), PromotionTypeEnum.FULL_DISCOUNT.name());
            promotionMap.put(key, promotion);
        }
        goodsIndex.setPromotionMap(promotionMap);
        updateIndex(goodsIndex);
    }

    /**
     * 移除需要移除的促销活动
     *
     * @param currentKey     当前操作的促销活动key
     * @param promotionMap   促销活动
     * @param needRemoveKeys 需移除的促销活动
     */
    private void removePromotionKey(String currentKey, Map<String, Object> promotionMap, String... needRemoveKeys) {
        if (CharSequenceUtil.containsAny(currentKey, needRemoveKeys)) {
            List<String> removeKeys = new ArrayList<>();
            for (String entry : promotionMap.keySet()) {
                for (String needRemoveKey : needRemoveKeys) {
                    if (entry.contains(needRemoveKey) && currentKey.contains(needRemoveKey)) {
                        removeKeys.add(entry);
                        break;
                    }
                }
            }
            promotionMap.keySet().removeAll(removeKeys);
        }
    }

    /**
     * 将商品关键字入库
     *
     * @param words 商品关键字
     */
    private void wordsToDb(String words) {
        try {
            // 是否有重复
            GoodsWords entity = goodsWordsService.getOne(new LambdaQueryWrapper<GoodsWords>().eq(GoodsWords::getWords, words));
            if (entity == null) {
                GoodsWords goodsWords = new GoodsWords();
                goodsWords.setWords(words);
                goodsWords.setWholeSpell(PinyinUtil.getPinyin(words, ""));
                goodsWords.setAbbreviate(PinyinUtil.getFirstLetter(words, ""));
                goodsWords.setType(GoodsWordsTypeEnum.SYSTEM.name());
                goodsWords.setSort(0);
                goodsWordsService.save(goodsWords);
            }
        } catch (MyBatisSystemException e) {
            log.error(words + "关键字已存在！");
        }
    }

    public List<EsGoodsIndex> searchList(String index) {
        SearchResponse searchResponse = search(index);
        SearchHit[] hits = searchResponse.getHits().getHits();
        List<EsGoodsIndex> goodsIndices = new ArrayList<>();
        Arrays.stream(hits).forEach(hit -> {
            Map<String, Object> sourceAsMap = hit.getSourceAsMap();
            EsGoodsIndex person = BeanUtil.mapToBean(sourceAsMap, EsGoodsIndex.class, false, CopyOptions.create());
            goodsIndices.add(person);
        });
        return goodsIndices;
    }

}
