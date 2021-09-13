package cn.lili.modules.search.serviceimpl;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ReflectUtil;
import cn.hutool.extra.pinyin.PinyinUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.elasticsearch.BaseElasticsearchService;
import cn.lili.elasticsearch.EsSuffix;
import cn.lili.elasticsearch.config.ElasticsearchProperties;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dos.GoodsWords;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.goods.entity.enums.GoodsWordsTypeEnum;
import cn.lili.modules.goods.service.GoodsWordsService;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.BasePromotion;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.modules.search.entity.dos.EsGoodsAttribute;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import cn.lili.modules.search.repository.EsGoodsIndexRepository;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.modules.search.service.EsGoodsSearchService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.assertj.core.util.IterableUtil;
import org.elasticsearch.action.bulk.BulkRequest;
import org.elasticsearch.action.update.UpdateRequest;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.client.indices.AnalyzeRequest;
import org.elasticsearch.client.indices.AnalyzeResponse;
import org.mybatis.spring.MyBatisSystemException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchPage;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 商品索引业务层实现
 *
 * @author paulG
 * @since 2020/10/14
 **/
@Slf4j
@Service
public class EsGoodsIndexServiceImpl extends BaseElasticsearchService implements EsGoodsIndexService {

    private static final String IGNORE_FIELD = "serialVersionUID,promotionMap,id,goodsId";

    private final Map<String, Field> fieldMap = ReflectUtil.getFieldMap(EsGoodsIndex.class);

    @Autowired
    private ElasticsearchProperties elasticsearchProperties;
    @Autowired
    private EsGoodsIndexRepository goodsIndexRepository;
    @Autowired
    private EsGoodsSearchService goodsSearchService;
    @Autowired
    private GoodsWordsService goodsWordsService;
    @Autowired
    private PromotionService promotionService;
    @Autowired
    private Cache<Object> cache;

    @Override
    public void addIndex(EsGoodsIndex goods) {
        //索引名称拼接
        String indexName = elasticsearchProperties.getIndexPrefix() + "_" + EsSuffix.GOODS_INDEX_NAME;
        try {
            //分词器分词
            AnalyzeRequest analyzeRequest = AnalyzeRequest.withIndexAnalyzer(indexName, "ik_max_word", goods.getGoodsName());
            AnalyzeResponse analyze = client.indices().analyze(analyzeRequest, RequestOptions.DEFAULT);
            List<AnalyzeResponse.AnalyzeToken> tokens = analyze.getTokens();

            if (goods.getAttrList() != null && !goods.getAttrList().isEmpty()) {
                //保存分词
                for (EsGoodsAttribute esGoodsAttribute : goods.getAttrList()) {
                    wordsToDb(esGoodsAttribute.getValue());
                }
            }
            //分析词条
            for (AnalyzeResponse.AnalyzeToken token : tokens) {
                //保存词条进入数据库
                wordsToDb(token.getTerm());
            }
            //生成索引
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
     * 更新商品索引的的部分属性（只填写更新的字段，不需要更新的字段不要填写）
     *
     * @param id    商品索引id
     * @param goods 更新后的购买数量
     */
    @Override
    public void updateIndex(String id, EsGoodsIndex goods) {
        EsGoodsIndex goodsIndex = this.findById(id);
        // 通过反射获取全部字段，在根据参数字段是否为空，设置要更新的字段
        for (Map.Entry<String, Field> entry : fieldMap.entrySet()) {
            Object fieldValue = ReflectUtil.getFieldValue(goods, entry.getValue());
            if (fieldValue != null && !IGNORE_FIELD.contains(entry.getKey())) {
                ReflectUtil.setFieldValue(goodsIndex, entry.getValue(), fieldValue);
            }
        }
        goodsIndexRepository.save(goods);
    }

    /**
     * 批量商品索引的的属性（ID 必填, 其他字段只填写更新的字段，不需要更新的字段不要填写。）
     *
     * @param goodsIndices 商品索引列表
     */
    @Override
    public void updateBulkIndex(List<EsGoodsIndex> goodsIndices) {
        try {
            //索引名称拼接
            String indexName = elasticsearchProperties.getIndexPrefix() + "_" + EsSuffix.GOODS_INDEX_NAME;

            BulkRequest request = new BulkRequest();

            for (EsGoodsIndex goodsIndex : goodsIndices) {
                UpdateRequest updateRequest = new UpdateRequest(indexName, goodsIndex.getId());

                JSONObject jsonObject = JSONUtil.parseObj(goodsIndex);
                jsonObject.set("releaseTime", goodsIndex.getReleaseTime().getTime());
                updateRequest.doc(jsonObject);
                request.add(updateRequest);
            }
            client.bulk(request, RequestOptions.DEFAULT);
        } catch (IOException e) {
            log.error("批量更新商品索引异常", e);
        }
    }

    @Override
    public void deleteIndex(EsGoodsIndex goods) {
        if (ObjectUtils.isEmpty(goods)) {
            //如果对象为空，则删除全量
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
        if (goodsIndexList == null || goodsIndexList.isEmpty()) {
            return;
        }
        //索引名称拼接
        String indexName = elasticsearchProperties.getIndexPrefix() + "_" + EsSuffix.GOODS_INDEX_NAME;

        //索引初始化，因为mapping结构问题：
        //但是如果索引已经自动生成过，这里就不会创建索引，设置mapping，所以这里决定在初始化索引的同时，将已有索引删除，重新创建

        //如果索引存在，则删除，重新生成。 这里应该有更优解。
        if (this.indexExist(indexName)) {
            deleteIndexRequest(indexName);
        }

        //如果索引不存在，则创建索引
        createIndexRequest(indexName);
        Map<String, Integer> resultMap = new HashMap<>(16);
        final String KEY_SUCCESS = "success";
        final String KEY_FAIL = "fail";
        final String KEY_PROCESSED = "processed";
        resultMap.put("total", goodsIndexList.size());
        resultMap.put(KEY_SUCCESS, 0);
        resultMap.put(KEY_FAIL, 0);
        resultMap.put(KEY_PROCESSED, 0);
        cache.put(CachePrefix.INIT_INDEX_PROCESS.getPrefix() + "", resultMap);
        cache.put(CachePrefix.INIT_INDEX_FLAG.getPrefix(), true);
        if (!goodsIndexList.isEmpty()) {
            goodsIndexRepository.deleteAll();
            for (EsGoodsIndex goodsIndex : goodsIndexList) {
                try {
                    addIndex(goodsIndex);
                    resultMap.put(KEY_SUCCESS, resultMap.get(KEY_SUCCESS) + 1);
                } catch (Exception e) {
                    log.error("商品{}生成索引错误！", goodsIndex);
                    resultMap.put(KEY_FAIL, resultMap.get(KEY_FAIL) + 1);
                }
                resultMap.put(KEY_PROCESSED, resultMap.get(KEY_PROCESSED) + 1);
                cache.put(CachePrefix.INIT_INDEX_PROCESS.getPrefix(), resultMap);
            }
        }
        cache.put(CachePrefix.INIT_INDEX_PROCESS.getPrefix(), resultMap);
        cache.put(CachePrefix.INIT_INDEX_FLAG.getPrefix(), false);
    }

    @Override
    public void updateEsGoodsIndex(String id, BasePromotion promotion, String key, Double price) {
        EsGoodsIndex goodsIndex = findById(id);
        if (goodsIndex != null) {
            //如果有促销活动开始，则将促销金额写入
            if (promotion.getPromotionStatus().equals(PromotionStatusEnum.START.name()) && price != null) {
                goodsIndex.setPromotionPrice(price);
            } else {
                //否则促销金额为商品原价
                goodsIndex.setPromotionPrice(goodsIndex.getPrice());
            }
            //更新索引
            this.updateGoodsIndexPromotion(goodsIndex, key, promotion);
        } else {
            log.error("更新索引商品促销信息失败！skuId 为 {} 的索引不存在！", id);
        }
    }

    @Override
    public void updateEsGoodsIndexByList(List<PromotionGoods> promotionGoodsList, BasePromotion promotion, String key) {
        if (promotionGoodsList != null) {
            //循环更新 促销商品索引
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
        List<EsGoodsIndex> goodsIndices = new ArrayList<>();
        //如果storeId不为空，则表示是店铺活动
        if (promotion.getStoreId() != null) {
            EsGoodsSearchDTO searchDTO = new EsGoodsSearchDTO();
            searchDTO.setStoreId(promotion.getStoreId());
            //查询出店铺商品
            SearchPage<EsGoodsIndex> esGoodsIndices = goodsSearchService.searchGoods(searchDTO, null);
            for (SearchHit<EsGoodsIndex> searchHit : esGoodsIndices.getContent()) {
                goodsIndices.add(searchHit.getContent());
            }
        } else {
            //否则是平台活动
            Iterable<EsGoodsIndex> all = goodsIndexRepository.findAll();
//           查询出全部商品
            goodsIndices = new ArrayList<>(IterableUtil.toCollection(all));
        }
        //更新商品索引
        for (EsGoodsIndex goodsIndex : goodsIndices) {
            this.updateGoodsIndexPromotion(goodsIndex, key, promotion);
        }
    }

    @Override
    public void deleteEsGoodsPromotionIndexByList(List<String> skuIds, PromotionTypeEnum promotionType) {
        //批量删除活动索引
        for (String skuId : skuIds) {
            EsGoodsIndex goodsIndex = findById(skuId);
            //商品索引不为空
            if (goodsIndex != null) {
                Map<String, Object> promotionMap = goodsIndex.getPromotionMap();
                if (promotionMap != null && !promotionMap.isEmpty()) {
                    //如果存在同类型促销活动删除
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

    @Override
    public void deleteEsGoodsPromotionByPromotionId(String skuId, String promotionId) {
        if (skuId != null) {
            EsGoodsIndex goodsIndex = findById(skuId);
            //商品索引不为空
            if (goodsIndex != null) {
                this.removePromotionByPromotionId(goodsIndex, promotionId);
            } else {
                log.error("更新索引商品促销信息失败！skuId 为 【{}】的索引不存在！", skuId);
            }
        } else {
            for (EsGoodsIndex goodsIndex : this.goodsIndexRepository.findAll()) {
                this.removePromotionByPromotionId(goodsIndex, promotionId);
            }
        }

    }

    /**
     * 从索引中删除指定促销活动id的促销活动
     *
     * @param goodsIndex  索引
     * @param promotionId 促销活动id
     */
    private void removePromotionByPromotionId(EsGoodsIndex goodsIndex, String promotionId) {
        Map<String, Object> promotionMap = goodsIndex.getPromotionMap();
        if (promotionMap != null && !promotionMap.isEmpty()) {
            //如果存在同类型促销活动删除
            List<String> collect = promotionMap.keySet().stream().filter(i -> i.split("-")[1].equals(promotionId)).collect(Collectors.toList());
            collect.forEach(promotionMap::remove);
            goodsIndex.setPromotionMap(promotionMap);
            updateIndex(goodsIndex);
        }
    }

    /**
     * 清除所有商品索引的无效促销活动
     */
    @Override
    public void cleanInvalidPromotion() {
        Iterable<EsGoodsIndex> all = goodsIndexRepository.findAll();
        for (EsGoodsIndex goodsIndex : all) {
            Map<String, Object> promotionMap = goodsIndex.getPromotionMap();
            //获取商品索引
            if (promotionMap != null && !promotionMap.isEmpty()) {
                //促销不为空则进行清洗
                for (Map.Entry<String, Object> entry : promotionMap.entrySet()) {
                    BasePromotion promotion = (BasePromotion) entry.getValue();
                    //判定条件为活动已结束
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

        //如果商品索引不为空，返回促销信息，否则返回空
        if (goodsIndex != null) {
            Map<String, Object> promotionMap = goodsIndex.getPromotionMap();
            if (promotionMap == null || promotionMap.isEmpty()) {
                return new HashMap<>(16);
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
        //如果没有促销信息，则返回新的
        if (promotionMap == null || promotionMap.isEmpty()) {
            return new ArrayList<>();
        }
        //对促销进行过滤
        List<String> keyCollect = promotionMap.keySet().stream().filter(i -> i.contains(promotionTypeEnum.name())).collect(Collectors.toList());
        List<String> promotionIds = new ArrayList<>();
        //写入促销id
        for (String key : keyCollect) {
            BasePromotion promotion = (BasePromotion) promotionMap.get(key);
            promotionIds.add(promotion.getId());
        }
        return promotionIds;
    }

    /**
     * 重置当前商品索引
     *
     * @param goodsSku       商品sku信息
     * @param goodsParamDTOS 商品参数
     * @return 商品索引
     */
    @Override
    public EsGoodsIndex resetEsGoodsIndex(GoodsSku goodsSku, List<GoodsParamsDTO> goodsParamDTOS) {
        EsGoodsIndex index = new EsGoodsIndex(goodsSku, goodsParamDTOS);
        //获取活动信息
        Map<String, Object> goodsCurrentPromotionMap = promotionService.getGoodsCurrentPromotionMap(index);
        //写入促销信息
        index.setPromotionMap(goodsCurrentPromotionMap);
        return index;
    }

    /**
     * 修改商品活动索引
     *
     * @param goodsIndex 商品索引
     * @param key        关键字
     * @param promotion  活动
     */
    private void updateGoodsIndexPromotion(EsGoodsIndex goodsIndex, String key, BasePromotion promotion) {
        log.info("修改商品活动索引");
        log.info("商品索引: {}", goodsIndex);
        log.info("关键字: {}", key);
        log.info("活动: {}", promotion);
        Map<String, Object> promotionMap;
        //数据非空处理，如果空给一个新的信息
        if (goodsIndex.getPromotionMap() == null || goodsIndex.getPromotionMap().isEmpty()) {
            promotionMap = new HashMap<>(1);
        } else {
            promotionMap = goodsIndex.getPromotionMap();
        }
        //如果活动已结束
        if (promotion.getPromotionStatus().equals(PromotionStatusEnum.END.name()) || promotion.getPromotionStatus().equals(PromotionStatusEnum.CLOSE.name())) {
            //如果存在活动
            if (promotionMap.containsKey(key)) {
                //删除活动
                promotionMap.remove(key);
            } else {
                //不存在则说明是秒杀活动，尝试删除秒杀信息
                this.removePromotionKey(key, promotionMap, PromotionTypeEnum.SECKILL.name());
            }
        } else {
            //添加促销活动前，如果是同一时间只可以有一个的活动，但商品索引的促销活动里存在其他（同一时间只可以有一个）的活动，则清除
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
        //判定是否需要移除
        if (CharSequenceUtil.containsAny(currentKey, needRemoveKeys)) {

            List<String> removeKeys = new ArrayList<>();
            //促销循环
            for (String entry : promotionMap.keySet()) {
                //需要移除则进行移除处理
                for (String needRemoveKey : needRemoveKeys) {
                    if (entry.contains(needRemoveKey) && currentKey.contains(needRemoveKey)) {
                        removeKeys.add(entry);
                        break;
                    }
                }
            }
            //移除促销信息
            removeKeys.forEach(promotionMap.keySet()::remove);
        }
    }

    /**
     * 将商品关键字入库
     *
     * @param words 商品关键字
     */
    private void wordsToDb(String words) {
        if (CharSequenceUtil.isEmpty(words)) {
            return;
        }
        try {
            //是否有重复
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

}
