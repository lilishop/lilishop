package cn.lili.modules.search.serviceimpl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.bean.copier.CopyOptions;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.extra.pinyin.PinyinUtil;
import cn.lili.common.elasticsearch.BaseElasticsearchService;
import cn.lili.common.elasticsearch.EsSuffix;
import cn.lili.config.elasticsearch.ElasticsearchProperties;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
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
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;

import java.io.IOException;
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
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

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
        //写入新的商品数据
        goodsIndex.setCommentNum(commentNum);
        goodsIndex.setHighPraiseNum(highPraiseNum);
        goodsIndex.setGrade(grade);
        this.updateIndex(goodsIndex);
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
            log.error("更新索引商品促销信息失败！skuId 为 【{}】的索引不存在！", id);
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
        List<EsGoodsIndex> goodsIndices;
        //如果storeId不为空，则表示是店铺活动
        if (promotion.getStoreId() != null) {
            EsGoodsSearchDTO searchDTO = new EsGoodsSearchDTO();
            searchDTO.setStoreId(promotion.getStoreId());
            //查询出店铺商品
            Page<EsGoodsIndex> esGoodsIndices = goodsSearchService.searchGoods(searchDTO, null);
            goodsIndices = esGoodsIndices.getContent();
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
     * @param goodsSku 商品sku信息
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
        this.addIndex(index);
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
