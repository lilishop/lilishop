package cn.lili.modules.search.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.map.MapUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.thread.ThreadUtil;
import cn.hutool.core.util.ReflectUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.RetryException;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.vo.PageVO;
import cn.lili.elasticsearch.BaseElasticsearchService;
import cn.lili.elasticsearch.EsSuffix;
import cn.lili.elasticsearch.config.ElasticsearchProperties;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsSalesModeEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.*;
import cn.lili.modules.promotion.entity.dos.BasePromotions;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.modules.search.entity.dos.CustomWords;
import cn.lili.modules.search.entity.dos.EsGoodsAttribute;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.entity.dto.EsDeleteDTO;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import cn.lili.modules.search.repository.EsGoodsIndexRepository;
import cn.lili.modules.search.service.CustomWordsService;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.modules.search.service.EsGoodsSearchService;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.elasticsearch.action.ActionListener;
import org.elasticsearch.action.bulk.BulkRequest;
import org.elasticsearch.action.bulk.BulkResponse;
import org.elasticsearch.action.update.UpdateRequest;
import org.elasticsearch.client.RequestOptions;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.index.query.TermQueryBuilder;
import org.elasticsearch.index.reindex.BulkByScrollResponse;
import org.elasticsearch.index.reindex.DeleteByQueryRequest;
import org.elasticsearch.index.reindex.UpdateByQueryRequest;
import org.elasticsearch.script.Script;
import org.elasticsearch.script.ScriptType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.SearchPage;
import org.springframework.data.elasticsearch.core.query.FetchSourceFilter;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.*;
import java.util.concurrent.TimeUnit;
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
    private static final String KEY_SUCCESS = "success";
    private static final String KEY_FAIL = "fail";
    private static final String KEY_PROCESSED = "processed";
    private final Map<String, Field> fieldMap = ReflectUtil.getFieldMap(EsGoodsIndex.class);
    @Autowired
    private ElasticsearchProperties elasticsearchProperties;
    @Autowired
    private EsGoodsIndexRepository goodsIndexRepository;
    @Autowired
    private EsGoodsSearchService goodsSearchService;
    @Autowired
    private PromotionService promotionService;

    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Autowired
    private CustomWordsService customWordsService;

    @Autowired
    private GoodsSkuService goodsSkuService;
    @Autowired
    private GoodsService goodsService;
    @Autowired
    private BrandService brandService;

    @Autowired
    private CategoryService categoryService;

    @Autowired
    private StoreGoodsLabelService storeGoodsLabelService;
    @Autowired
    private EsGoodsSearchService esGoodsSearchService;
    @Autowired
    private Cache<Object> cache;
    /**
     * rocketMq
     */
    @Autowired
    private RocketMQTemplate rocketMQTemplate;
    /**
     * rocketMq配置
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    @Autowired
    private ElasticsearchOperations restTemplate;

    /**
     * 去除 重复元素
     *
     * @param list
     * @return
     */
    public static void removeDuplicate(List<String> list) {
        HashSet<String> h = new HashSet<>(list);
        list.clear();
        list.addAll(h);
    }

    @Override
    public Boolean deleteGoodsDown() {
        List<Goods> goodsList = goodsService.list(new LambdaQueryWrapper<Goods>().eq(Goods::getMarketEnable, GoodsStatusEnum.DOWN.name()));
        for (Goods goods : goodsList) {
            this.deleteIndex(
                    EsDeleteDTO.builder()
                            .queryFields(MapUtil.builder(new HashMap<String, Object>()).put("goodsId", goods.getId()).build())
                            .clazz(EsGoodsIndex.class)
                            .build());
        }
        return true;
    }

    @Override
    public Boolean delSkuIndex() {
        PageVO pageVO = new PageVO();
        EsGoodsSearchDTO goodsSearchParams = new EsGoodsSearchDTO();
        log.error("开始");
        try {
            List<Object> searchFilters = new ArrayList<>();
            for (int i = 1; ; i++) {

                log.error("第" + i + "页");

                pageVO.setPageSize(1000);
                pageVO.setPageNumber(i);
                pageVO.setNotConvert(true);
                pageVO.setSort("_id");
                pageVO.setOrder("asc");

                NativeSearchQueryBuilder searchQueryBuilder = esGoodsSearchService.createSearchQueryBuilder(goodsSearchParams, pageVO);


                searchQueryBuilder.withSourceFilter(new FetchSourceFilter(new String[]{"_id"}, null));

                Pageable pageable = PageRequest.of(0, 1000);
                //分页
                searchQueryBuilder.withPageable(pageable);
                NativeSearchQuery query = searchQueryBuilder.build();
                EsGoodsSearchDTO searchDTO = new EsGoodsSearchDTO();
                SearchPage<EsGoodsIndex> searchHits = goodsSearchService.searchGoods(query, EsGoodsIndex.class);

                if (searchHits == null || searchHits.isEmpty()) {
                    break;
                }

                List<String> idList = searchHits.getSearchHits().stream().map(SearchHit::getContent).map(EsGoodsIndex::getId).collect(Collectors.toList());
                LambdaQueryWrapper<GoodsSku> queryWrapper = new LambdaQueryWrapper<>();
                queryWrapper.select(GoodsSku::getId);
                queryWrapper.in(GoodsSku::getId, idList);
                List<GoodsSku> goodsSkus = goodsSkuService.list(queryWrapper);

                idList.forEach(id -> {
                    if (goodsSkus.stream().noneMatch(goodsSku -> goodsSku.getId().equals(id))) {
                        log.error("[{}]不存在，进行删除", id);
                        this.deleteIndexById(id);
                    }
                });


                if (!searchHits.getSearchHits().getSearchHit(idList.size() -1).getSortValues().isEmpty()) {
                    searchFilters = searchHits.getSearchHits().getSearchHit(idList.size() -1).getSortValues();
                }

            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        System.out.println("结束了");
        return true;
    }

    @Override
    public Boolean goodsCache() {
        GoodsSearchParams searchParams = new GoodsSearchParams();
        searchParams.setAuthFlag(GoodsAuthEnum.PASS.name());
        searchParams.setMarketEnable(GoodsStatusEnum.UPPER.name());

        for (int i = 1; ; i++) {
            try {
                IPage<Goods> pagePage = new Page<>();
                searchParams.setPageSize(1000);
                searchParams.setPageNumber(i);
                pagePage = goodsService.queryByParams(searchParams);

                if (pagePage == null || CollUtil.isEmpty(pagePage.getRecords())) {
                    break;
                }
                for (Goods goods : pagePage.getRecords()) {
                    cache.remove(CachePrefix.GOODS.getPrefix() + goods.getId());
                    goodsService.getGoodsVO(goods.getId());
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }


        for (int i = 1; ; i++) {
            try {
                IPage<GoodsSkuDTO> skuIPage = new Page<>();
                searchParams.setPageSize(1000);
                searchParams.setPageNumber(i);
                skuIPage = goodsSkuService.getGoodsSkuDTOByPage(PageUtil.initPage(searchParams),
                        searchParams.queryWrapper());

                if (skuIPage == null || CollUtil.isEmpty(skuIPage.getRecords())) {
                    break;
                }
                for (GoodsSkuDTO goodsSkuDTO : skuIPage.getRecords()) {
                    GoodsSku goodsSku = goodsSkuService.getById(goodsSkuDTO.getId());
                    cache.put(GoodsSkuService.getCacheKeys(goodsSkuDTO.getId()), goodsSku,600L);
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return true;
    }

    @Override
    public void init() {
        //获取索引任务标识
        Boolean flag = (Boolean) cache.get(CachePrefix.INIT_INDEX_FLAG.getPrefix());
        //为空则默认写入没有任务
        if (flag == null) {
            cache.put(CachePrefix.INIT_INDEX_FLAG.getPrefix(), false);
        }
        //有正在初始化的任务，则提示异常
        if (Boolean.TRUE.equals(flag)) {
            throw new ServiceException(ResultCode.INDEX_BUILDING);
        }

        //初始化标识
        cache.put(CachePrefix.INIT_INDEX_PROCESS.getPrefix(), null);
        cache.put(CachePrefix.INIT_INDEX_FLAG.getPrefix(), true, 10L, TimeUnit.MINUTES);


        ThreadUtil.execAsync(() -> {
            try {

                QueryWrapper<GoodsSkuDTO> skuQueryWrapper = new QueryWrapper<>();
                skuQueryWrapper.eq("gs.auth_flag", GoodsAuthEnum.PASS.name());
                skuQueryWrapper.eq("gs.market_enable", GoodsStatusEnum.UPPER.name());
                skuQueryWrapper.eq("gs.delete_flag", false);
                skuQueryWrapper.gt("gs.quantity", 0);


                Map<String, Long> resultMap = (Map<String, Long>) cache.get(CachePrefix.INIT_INDEX_PROCESS.getPrefix());

                if (CollUtil.isEmpty(resultMap)) {
                    QueryWrapper<GoodsSku> skuCountQueryWrapper = new QueryWrapper<>();
                    skuCountQueryWrapper.eq("auth_flag", GoodsAuthEnum.PASS.name());
                    skuCountQueryWrapper.eq("market_enable", GoodsStatusEnum.UPPER.name());
                    skuCountQueryWrapper.eq("delete_flag", false);
                    skuCountQueryWrapper.gt("quantity", 0);
                    resultMap = new HashMap<>();
                    resultMap.put(KEY_SUCCESS, 0L);
                    resultMap.put(KEY_FAIL, 0L);
                    resultMap.put(KEY_PROCESSED, 0L);
                    resultMap.put("total", this.goodsSkuService.count(skuCountQueryWrapper));
                    cache.put(CachePrefix.INIT_INDEX_PROCESS.getPrefix(), resultMap);
                }

                for (int i = 1; ; i++) {
                    List<EsGoodsIndex> esGoodsIndices = new ArrayList<>();
                    Page<GoodsSkuDTO> skuPage = new Page<>(i, 2000);
                    IPage<GoodsSkuDTO> skuIPage = goodsSkuService.getGoodsSkuDTOByPage(skuPage, skuQueryWrapper);

                    if (skuIPage == null || CollUtil.isEmpty(skuIPage.getRecords())) {
                        break;
                    }
                    List<String> skuIds = skuIPage.getRecords().stream().map(GoodsSku::getId).collect(Collectors.toList());
                    List<PromotionGoods> skuValidPromotions = promotionGoodsService.findSkuValidPromotions(skuIds);

                    List<String> brandIds = new ArrayList<>();

                    List<String> categoryPaths = new ArrayList<>();

                    List<String> storeCategoryPaths = new ArrayList<>();

                    for (GoodsSkuDTO goodsSkuDTO : skuIPage.getRecords()) {
                        if (CharSequenceUtil.isNotEmpty(goodsSkuDTO.getBrandId())) {
                            brandIds.add(goodsSkuDTO.getBrandId());
                        }
                        if (CharSequenceUtil.isNotEmpty(goodsSkuDTO.getStoreCategoryPath())) {
                            storeCategoryPaths.addAll(Arrays.asList(goodsSkuDTO.getStoreCategoryPath().split(",")));
                        }
                        if (CharSequenceUtil.isNotEmpty((goodsSkuDTO.getCategoryPath()))) {
                            categoryPaths.addAll(Arrays.asList(goodsSkuDTO.getCategoryPath().split(",")));
                        }
                    }

                    List<Map<String, Object>> brandList = new ArrayList<>();
                    if (CollUtil.isNotEmpty(brandIds)) {
                        brandList = this.brandService.getBrandsMapsByCategory(CollUtil.distinct(brandIds), "id,name,logo");
                    }
                    List<Map<String, Object>> categoryList = new ArrayList<>();
                    if (CollUtil.isNotEmpty(categoryPaths)) {
                        categoryList = this.categoryService.listMapsByIdsOrderByLevel(CollUtil.distinct(categoryPaths), "id,name");
                    }
                    List<Map<String, Object>> storeCategoryList = new ArrayList<>();
                    if (CollUtil.isNotEmpty(storeCategoryPaths)) {
                        storeCategoryList = this.storeGoodsLabelService.listMapsByStoreIds(CollUtil.distinct(storeCategoryPaths), "id,label_name");
                    }

                    for (GoodsSkuDTO goodsSku : skuIPage.getRecords()) {
                        int skuSource = 100;
                        EsGoodsIndex esGoodsIndex = wrapperEsGoodsIndex(goodsSku, brandList, categoryList, storeCategoryList);
                        long count = esGoodsIndices.stream().filter(j -> j.getGoodsId().equals(esGoodsIndex.getGoodsId())).count();
                        if (count >= 1) {
                            skuSource -= count;
                        }
                        if (skuSource <= 0) {
                            skuSource = 1;
                        }
                        esGoodsIndex.setSkuSource(skuSource);


                        //设置促销信息
                        List<PromotionGoods> promotionGoods = skuValidPromotions.stream()
                                .filter(j ->
                                        (CharSequenceUtil.isNotEmpty(j.getSkuId()) && j.getSkuId().equals(goodsSku.getId())) ||
                                                (j.getScopeType().equals(PromotionsScopeTypeEnum.ALL.name()) && "0".equals(j.getStoreId())) ||
                                                (j.getScopeType().equals(PromotionsScopeTypeEnum.ALL.name()) && j.getStoreId().equals(esGoodsIndex.getStoreId())) ||
                                                (j.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_GOODS_CATEGORY.name()) && "0".equals(j.getStoreId()) && j.getScopeId().contains(goodsSku.getCategoryPath()))||
                                                (j.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_GOODS_CATEGORY.name()) && j.getStoreId().equals(goodsSku.getStoreId()) && j.getScopeId().contains(goodsSku.getCategoryPath()))
                                )
                                .collect(Collectors.toList());
                        if (CollUtil.isNotEmpty(promotionGoods)) {
                            esGoodsIndex.setPromotionMapJson(JSONUtil.toJsonStr(promotionService.wrapperPromotionMapList(promotionGoods)));
                        }

                        esGoodsIndices.add(esGoodsIndex);
                        //库存锁是在redis做的，所以生成索引，同时更新一下redis中的库存数量
                        cache.put(GoodsSkuService.getStockCacheKey(goodsSku.getId()), goodsSku.getQuantity());
                    }

                    //批量插入索引，如果为第一次则删除原索引并创建新索引
                    this.initIndex(esGoodsIndices, i == 1);
                }

                cache.put(CachePrefix.INIT_INDEX_FLAG.getPrefix(), false);

                //初始化商品索引
            } catch (Exception e) {
                log.error("商品索引生成异常：", e);
                //如果出现异常，则将进行中的任务标识取消掉，打印日志
                cache.put(CachePrefix.INIT_INDEX_PROCESS.getPrefix(), null);
                cache.put(CachePrefix.INIT_INDEX_FLAG.getPrefix(), false);
            }
        });

    }

    @Override
    public Map<String, Long> getProgress() {
        Map<String, Long> map = (Map<String, Long>) cache.get(CachePrefix.INIT_INDEX_PROCESS.getPrefix());
        if (map == null) {
            return Collections.emptyMap();
        }
        Boolean flag = (Boolean) cache.get(CachePrefix.INIT_INDEX_FLAG.getPrefix());
        map.put("flag", Boolean.TRUE.equals(flag) ? 1L : 0L);
        return map;
    }

    @Override
    public void initIndex() {
        //索引名称拼接
        String indexName = this.getIndexName();

        //索引初始化，因为mapping结构问题：
        //但是如果索引已经自动生成过，这里就不会创建索引，设置mapping，所以这里决定在初始化索引的同时，将已有索引删除，重新创建

        boolean indexExist = this.indexExist(indexName);
        log.info("检测 {} 索引结构是否存在：{}", indexName, indexExist);
        if (!indexExist) {

            log.info("初始化索引结构 {}", indexName);
            //如果索引不存在，则创建索引
            createIndexRequest(indexName);
        }

    }

    @Override
    public void addIndex(EsGoodsIndex goods) {
        try {
            //分词器分词
            this.analyzeAndSaveWords(goods);
            //生成索引
            goodsIndexRepository.save(goods);
        } catch (Exception e) {
            log.error("为商品[" + goods.getGoodsName() + "]生成索引异常", e);
        }
    }

    /**
     * 添加商品索引
     *
     * @param goods 商品索引信息
     */
    @Override
    public void addIndex(List<EsGoodsIndex> goods) {
        try {
            for (EsGoodsIndex esGoodsIndex : goods) {
                this.analyzeAndSaveWords(esGoodsIndex);
            }
            goodsIndexRepository.saveAll(goods);
        } catch (Exception e) {
            log.error("批量为商品生成索引异常", e);
        }
    }

    @Override
    public void updateIndex(EsGoodsIndex goods) {
        this.analyzeAndSaveWords(goods);
        goodsIndexRepository.save(goods);
    }

    /**
     * 商品分词
     *
     * @param goods 商品信息
     */
    private void analyzeAndSaveWords(EsGoodsIndex goods) {
        try {
            List<String> keywordsList = new ArrayList<>();
            //根据商品参数分词
            if (goods.getAttrList() != null && !goods.getAttrList().isEmpty()) {
                //保存分词
                for (EsGoodsAttribute esGoodsAttribute : goods.getAttrList()) {
                    if (keywordsList.stream().noneMatch(i -> i.toLowerCase(Locale.ROOT).equals(esGoodsAttribute.getValue().toLowerCase(Locale.ROOT)))) {
                        keywordsList.add(esGoodsAttribute.getValue());
                    }
                }
            }
            //根据商品名称生成分词
            keywordsList.add(goods.getGoodsName().substring(0, Math.min(goods.getGoodsName().length(), 10)));

            //去除重复词
            removeDuplicate(keywordsList);
            //入库自定义分词
            List<CustomWords> customWordsArrayList = new ArrayList<>();
            keywordsList.forEach(item -> customWordsArrayList.add(new CustomWords(item)));
            //这里采用先批量删除再插入的方法，故意这么做。否则需要挨个匹配是否存在，性能消耗更大
            if (CollUtil.isNotEmpty(customWordsArrayList)) {
                customWordsService.insertBatchCustomWords(customWordsArrayList);
            }
        } catch (Exception e) {
            log.info(goods + "自定义分词错误", e);
        }
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
        goodsIndexRepository.save(goodsIndex);
    }

    /**
     * 更新商品索引的的部分属性（只填写更新的字段，不需要更新的字段不要填写）
     *
     * @param queryFields  查询字段
     * @param updateFields 更新字段
     */
    @Override
    public void updateIndex(Map<String, Object> queryFields, Map<String, Object> updateFields) {
        UpdateByQueryRequest update = new UpdateByQueryRequest(getIndexName());
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        for (Map.Entry<String, Object> entry : queryFields.entrySet()) {
            TermQueryBuilder termQueryBuilder = new TermQueryBuilder(entry.getKey(), entry.getValue());
            queryBuilder.filter(termQueryBuilder);
        }
        update.setQuery(queryBuilder);
        StringBuilder script = new StringBuilder();
        for (Map.Entry<String, Object> entry : updateFields.entrySet()) {
            script.append("ctx._source.").append(entry.getKey()).append("=").append("'").append(entry.getValue()).append("'").append(";");
        }
        update.setScript(new Script(script.toString()));
        update.setConflicts("proceed");

        this.client.updateByQueryAsync(update, RequestOptions.DEFAULT, new ActionListener<BulkByScrollResponse>() {
            @Override
            public void onResponse(BulkByScrollResponse bulkByScrollResponse) {
                if (bulkByScrollResponse.getVersionConflicts() > 0) {
                    throw new RetryException("更新商品索引失败，es内容版本冲突");
                }
            }

            @Override
            public void onFailure(Exception e) {
                log.error("更新商品索引异常", e);
            }
        });
    }

    /**
     * 批量商品索引的的属性（ID 必填, 其他字段只填写更新的字段，不需要更新的字段不要填写。）
     *
     * @param goodsIndices 商品索引列表
     */
    @Override
    public void updateBulkIndex(List<EsGoodsIndex> goodsIndices) {
        //索引名称拼接
        String indexName = getIndexName();

        BulkRequest request = new BulkRequest();

        for (EsGoodsIndex goodsIndex : goodsIndices) {
            UpdateRequest updateRequest = new UpdateRequest(indexName, goodsIndex.getId());

            JSONObject jsonObject = JSONUtil.parseObj(goodsIndex);
            jsonObject.set("releaseTime", goodsIndex.getReleaseTime());
            updateRequest.doc(jsonObject);
            request.add(updateRequest);
        }
        this.client.bulkAsync(request, RequestOptions.DEFAULT, new ActionListener<BulkResponse>() {
            @Override
            public void onResponse(BulkResponse bulkItemResponses) {
                // 判断索引如果不存在的处理
                log.info("批量更新商品索引结果：{}", bulkItemResponses.buildFailureMessage());
            }

            @Override
            public void onFailure(Exception e) {
                log.error("批量更新商品索引异常", e);
            }
        });
    }

    /**
     * 删除索引
     *
     * @param queryFields 查询条件
     */
    @Override
    public void deleteIndex(Map<String, Object> queryFields) {
        BoolQueryBuilder boolQueryBuilder = QueryBuilders.boolQuery();
        for (Map.Entry<String, Object> entry : queryFields.entrySet()) {
            boolQueryBuilder.filter(QueryBuilders.termsQuery(entry.getKey(), entry.getValue().toString()));
        }

        DeleteByQueryRequest deleteByQueryRequest = new DeleteByQueryRequest();
        deleteByQueryRequest.setQuery(boolQueryBuilder);
        deleteByQueryRequest.indices(getIndexName());
        deleteByQueryRequest.setConflicts("proceed");

        try {
            this.client.deleteByQuery(deleteByQueryRequest, RequestOptions.DEFAULT);
        } catch (IOException e) {
            log.error("删除索引出现异常", e);
        }

    }

    /**
     * 删除索引
     *
     * @param esDeleteDTO 删除索引参数
     */
    private void deleteIndex(EsDeleteDTO esDeleteDTO) {
        if (esDeleteDTO.getIds() != null && !esDeleteDTO.getIds().isEmpty()) {
            this.deleteIndexByIds(esDeleteDTO.getIds());
        }
        if (esDeleteDTO.getQueryFields() != null && esDeleteDTO.getQueryFields().size() > 0) {
            this.deleteIndex(esDeleteDTO.getQueryFields());
        }
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

    /**
     * 删除索引
     *
     * @param ids 商品索引id集合
     */
    @Override
    public void deleteIndexByIds(List<String> ids) {
        NativeSearchQueryBuilder queryBuilder = new NativeSearchQueryBuilder();
        queryBuilder.withQuery(QueryBuilders.termsQuery("id", ids.toArray()));
        this.restTemplate.delete(queryBuilder.build(), EsGoodsIndex.class);
    }

    @Override
    public void initIndex(List<EsGoodsIndex> goodsIndexList, boolean regeneratorIndex) {
        if (goodsIndexList == null || goodsIndexList.isEmpty()) {
            //初始化标识
            cache.put(CachePrefix.INIT_INDEX_PROCESS.getPrefix(), null);
            cache.put(CachePrefix.INIT_INDEX_FLAG.getPrefix(), false);
            return;
        }
        //索引名称拼接
        String indexName = this.getIndexName();

        //索引初始化，因为mapping结构问题：
        //但是如果索引已经自动生成过，这里就不会创建索引，设置mapping，所以这里决定在初始化索引的同时，将已有索引删除，重新创建

        //如果索引存在，则删除，重新生成。 这里应该有更优解。
        boolean indexExist = this.indexExist(indexName);
        if (regeneratorIndex || !indexExist) {
            if (indexExist) {
                this.deleteIndexRequest(indexName);
            }
            //如果索引不存在，则创建索引
            this.createIndexRequest(indexName);
        }

        Map<String, Long> resultMap = (Map<String, Long>) cache.get(CachePrefix.INIT_INDEX_PROCESS.getPrefix());
        if (!goodsIndexList.isEmpty()) {
            for (EsGoodsIndex goodsIndex : goodsIndexList) {
                try {
                    log.info("生成商品索引：{}", goodsIndex);
                    this.addIndex(goodsIndex);
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
    }

    @Override
    public UpdateRequest updateEsGoodsIndexPromotions(String id, BasePromotions promotion, String key) {
        EsGoodsIndex goodsIndex = findById(id);
        if (goodsIndex != null) {
            // 批发商品不参与促销（除优惠券和满减）
            if (PromotionTools.isPromotionsTypeNeedsToChecked(key) && GoodsSalesModeEnum.WHOLESALE.name().equals(goodsIndex.getSalesModel())) {
                return null;
            }
            //更新索引
            return this.updateGoodsIndexPromotion(goodsIndex, key, promotion);
        } else {
            log.error("更新索引商品促销信息失败！skuId 为 {} 的索引不存在！", id);
            return null;
        }
    }

    /**
     * 更新商品索引的促销信息
     *
     * @param ids       skuId集合
     * @param promotion 促销信息
     * @param key       促销信息的key
     */
    @Override
    public void updateEsGoodsIndexPromotions(List<String> ids, BasePromotions promotion, String key) {
        BulkRequest bulkRequest = new BulkRequest();
        log.info("更新商品索引的促销信息----------");
        log.info("商品ids: {}", ids);
        log.info("活动: {}", promotion);
        log.info("key: {}", key);
        for (String id : ids) {
            UpdateRequest updateRequest = this.updateEsGoodsIndexPromotions(id, promotion, key);
            if (updateRequest != null) {
                bulkRequest.add(updateRequest);
            }
        }
        this.executeBulkUpdateRequest(bulkRequest);
    }


    @Override
    public void updateEsGoodsIndexByList(List<PromotionGoods> promotionGoodsList, BasePromotions promotion, String key) {
        BulkRequest bulkRequest = new BulkRequest();
        log.info("修改商品活动索引");
        log.info("促销商品信息: {}", promotionGoodsList);
        log.info("活动关键字: {}", key);
        log.info("活动: {}", promotion);
        if (promotionGoodsList != null) {
            //循环更新 促销商品索引
            for (PromotionGoods promotionGoods : promotionGoodsList) {
                promotion.setStartTime(promotionGoods.getStartTime());
                promotion.setEndTime(promotionGoods.getEndTime());
                UpdateRequest updateRequest = this.updateEsGoodsIndexPromotions(promotionGoods.getSkuId(), promotion, key);
                if (updateRequest != null) {
                    bulkRequest.add(updateRequest);
                }
            }
        }
        this.executeBulkUpdateRequest(bulkRequest);
    }

    /**
     * 更新全部商品索引的促销信息
     *
     * @param promotion 促销信息
     * @param key       促销信息的key
     */
    @Override
    public void updateEsGoodsIndexAllByList(BasePromotions promotion, String key) {
        ThreadUtil.execAsync(() -> this.executeUpdateEsGoodsIndexAll(promotion, key));

    }

    private void executeUpdateEsGoodsIndexAll(BasePromotions promotion, String key) {
        for (int i = 0; ; i++) {
            List<String> skuIds;
            PageVO pageVO = new PageVO();
            pageVO.setPageNumber(i);
            pageVO.setPageSize(1000);
            EsGoodsSearchDTO searchDTO = new EsGoodsSearchDTO();
            if (PromotionTools.isPromotionsTypeNeedsToChecked(key)) {
                searchDTO.setSalesModel(GoodsSalesModeEnum.RETAIL.name());
            }
            //如果storeId不为空，则表示是店铺活动
            if (promotion.getStoreId() != null && !promotion.getStoreId().equals(PromotionTools.PLATFORM_ID)) {
                searchDTO.setStoreId(promotion.getStoreId());
            }

            //查询出店铺商品
            SearchPage<EsGoodsIndex> esGoodsIndices = goodsSearchService.searchGoods(searchDTO, pageVO);

            skuIds = esGoodsIndices.isEmpty() ? new ArrayList<>() :
                    esGoodsIndices.getContent().stream().map(SearchHit::getId).collect(Collectors.toList());
            if (skuIds.isEmpty()) {
                break;
            }
            this.deleteEsGoodsPromotionByPromotionKey(skuIds, key);
            this.updateEsGoodsIndexPromotions(skuIds, promotion, key);
        }
    }

    @Override
    public void deleteEsGoodsPromotionByPromotionKey(List<String> skuIds, String promotionsKey) {
        BulkRequest bulkRequest = new BulkRequest();
        log.info("删除商品活动索引");
        log.info("商品skuIds: {}", skuIds);
        log.info("活动Key: {}", promotionsKey);
        if (skuIds == null || skuIds.isEmpty()) {
            return;
        }
        for (String skuId : skuIds) {
            EsGoodsIndex goodsIndex = findById(skuId);
            //商品索引不为空
            if (goodsIndex != null) {
                UpdateRequest updateRequest = this.removePromotionByPromotionKey(goodsIndex, promotionsKey);
                if (updateRequest != null) {
                    bulkRequest.add(updateRequest);
                }
            } else {
                log.error("更新索引商品促销信息失败！skuId 为 【{}】的索引不存在！", skuId);
            }
        }
        this.executeBulkUpdateRequest(bulkRequest);
    }

    /**
     * 删除索引中指定的促销活动id的促销活动
     *
     * @param promotionsKey 促销活动Key
     */
    @Override
    public void deleteEsGoodsPromotionByPromotionKey(String promotionsKey) {
        ThreadUtil.execAsync(() -> {
            for (int i = 0; ; i++) {
                BulkRequest bulkRequest = new BulkRequest();

                NativeSearchQueryBuilder nativeSearchQueryBuilder = new NativeSearchQueryBuilder();
                nativeSearchQueryBuilder.withQuery(QueryBuilders.matchAllQuery());
                nativeSearchQueryBuilder.withPageable(PageRequest.of(i, 1000));
                try {
                    SearchHits<EsGoodsIndex> esGoodsIndices = this.restTemplate.search(nativeSearchQueryBuilder.build(), EsGoodsIndex.class);
                    if (esGoodsIndices.isEmpty() || esGoodsIndices.getSearchHits().isEmpty()) {
                        break;
                    }
                    for (SearchHit<EsGoodsIndex> searchHit : esGoodsIndices.getSearchHits()) {
                        EsGoodsIndex goodsIndex = searchHit.getContent();
                        UpdateRequest updateRequest = this.removePromotionByPromotionKey(goodsIndex, promotionsKey);
                        if (updateRequest != null) {
                            bulkRequest.add(updateRequest);
                        }
                    }
                    this.executeBulkUpdateRequest(bulkRequest);
                } catch (Exception e) {
                    log.error("删除索引中指定的促销活动id的促销活动失败！key: {}", promotionsKey, e);
                    return;
                }
            }
        });
    }

    /**
     * 从索引中删除指定促销活动id的促销活动
     *
     * @param goodsIndex    索引
     * @param promotionsKey 促销活动key
     */
    private UpdateRequest removePromotionByPromotionKey(EsGoodsIndex goodsIndex, String promotionsKey) {
        Map<String, Object> promotionMap = goodsIndex.getOriginPromotionMap();
        if (promotionMap != null && !promotionMap.isEmpty()) {
            //如果存在同促销ID的活动删除
            Map<String, Object> filterPromotionMap = promotionMap.entrySet().stream().filter(i -> !i.getKey().equals(promotionsKey)).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
            return this.getGoodsIndexPromotionUpdateRequest(goodsIndex.getId(), filterPromotionMap);
        }
        return null;
    }

    /**
     * 清除所有商品索引的无效促销活动
     */
    @Override
    public void cleanInvalidPromotion() {
        ThreadUtil.execAsync(this::executeCleanInvalidPromotions);
    }

    private void executeCleanInvalidPromotions() {
        for (int i = 0; ; i++) {
            org.springframework.data.domain.Page<EsGoodsIndex> all = goodsIndexRepository.findAll(PageRequest.of(i, 1000));
            if (all.isEmpty()) {
                break;
            }
            for (EsGoodsIndex goodsIndex : all.toList()) {
                Map<String, Object> promotionMap = goodsIndex.getOriginPromotionMap();
                //获取商品索引
                if (promotionMap != null && !promotionMap.isEmpty()) {
                    //促销不为空则进行清洗
                    promotionMap.entrySet().removeIf(j -> {
                        JSONObject promotionJson = JSONUtil.parseObj(j.getValue());
                        BasePromotions promotion = promotionJson.toBean(BasePromotions.class);
                        return promotion.getEndTime() != null && promotion.getEndTime().getTime() < DateUtil.date().getTime();
                    });

                    // 更新回 goodsIndex 对象
                    goodsIndex.setPromotionMapJson(JSONUtil.toJsonStr(promotionMap));
                }
            }
            goodsIndexRepository.saveAll(all);
        }
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
            Map<String, Object> promotionMap = goodsIndex.getOriginPromotionMap();
            if (promotionMap == null || promotionMap.isEmpty()) {
                return new HashMap<>(16);
            }
            return promotionMap;
        }
        return new HashMap<>();
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
            BasePromotions promotion = (BasePromotions) promotionMap.get(key);
            promotionIds.add(promotion.getId());
        }
        return promotionIds;
    }

    /**
     * 获取重置的商品索引
     *
     * @param goodsSku       商品sku信息
     * @param goodsParamDTOS 商品参数
     * @return 商品索引
     */
    @Override
    public EsGoodsIndex getResetEsGoodsIndex(GoodsSku goodsSku, List<GoodsParamsDTO> goodsParamDTOS) {
        EsGoodsIndex index = new EsGoodsIndex(goodsSku, goodsParamDTOS);
        //获取活动信息
        Map<String, Object> goodsCurrentPromotionMap = promotionService.getGoodsSkuPromotionMap(index.getStoreId(), index.getId());
        //写入促销信息
        index.setPromotionMapJson(JSONUtil.toJsonStr(goodsCurrentPromotionMap));

        //发送mq消息
        String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.RESET_GOODS_INDEX.name();
        rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(Collections.singletonList(index)), RocketmqSendCallbackBuilder.commonCallback());
        return index;
    }

    /**
     * 修改商品活动索引
     *
     * @param goodsIndex 商品索引
     * @param key        关键字
     * @param promotion  活动
     */
    private UpdateRequest updateGoodsIndexPromotion(EsGoodsIndex goodsIndex, String key, BasePromotions promotion) {
        Map<String, Object> promotionMap;
        //数据非空处理，如果空给一个新的信息
        if (goodsIndex.getOriginPromotionMap() == null || goodsIndex.getOriginPromotionMap().isEmpty()) {
            promotionMap = new HashMap<>(1);
        } else {
            promotionMap = goodsIndex.getOriginPromotionMap();
        }
//        log.info("ES修改商品活动索引-原商品索引信息:{}", goodsIndex);
//        log.info("ES修改商品活动索引-原商品索引活动信息:{}", promotionMap);
        //如果活动已结束
        //如果存在活动
        if (promotion.getPromotionStatus().equals(PromotionsStatusEnum.END.name()) || promotion.getPromotionStatus().equals(PromotionsStatusEnum.CLOSE.name())) {
            //删除活动
            promotionMap.remove(key);
        } else {
            promotionMap.put(key, promotion);
        }
//        log.info("ES修改商品活动索引-过滤后商品索引活动信息:{}", promotionMap);
        return this.getGoodsIndexPromotionUpdateRequest(goodsIndex.getId(), promotionMap);
    }

    /**
     * 以更新部分字段的方式更新索引促销信息
     *
     * @param id           索引id
     * @param promotionMap 促销信息
     */
    private UpdateRequest getGoodsIndexPromotionUpdateRequest(String id, Map<String, Object> promotionMap) {
        UpdateRequest updateRequest = new UpdateRequest();
        updateRequest.index(getIndexName());
        updateRequest.id(id);
        updateRequest.retryOnConflict(3);
//        updateRequest.version(promotionMap.size());
        Map<String, Object> params = new HashMap<>();
        params.put("promotionMap", JSONUtil.toJsonStr(promotionMap));
        Script script = new Script(ScriptType.INLINE, "painless", "ctx._source.promotionMapJson=params.promotionMap;", params);
        updateRequest.script(script);
        return updateRequest;
    }

    /**
     * 执行批量更新商品索引
     *
     * @param bulkRequest 批量请求
     */
    private void executeBulkUpdateRequest(BulkRequest bulkRequest) {
        if (bulkRequest.requests().isEmpty()) {
            return;
        }
        this.client.bulkAsync(bulkRequest, RequestOptions.DEFAULT, new ActionListener<BulkResponse>() {
            @Override
            public void onResponse(BulkResponse bulkItemResponses) {
                if (bulkItemResponses.hasFailures()) {
                    log.info("批量更新商品索引的促销信息中出现部分异常：{}", bulkItemResponses.buildFailureMessage());
                } else {
                    log.info("批量更新商品索引的促销信息结果：{}", bulkItemResponses.status());
                }
            }

            @Override
            public void onFailure(Exception e) {
                log.error("批量更新商品索引的促销信息出现异常！", e);
            }
        });

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

    private String getIndexName() {
        //索引名称拼接
        return elasticsearchProperties.getIndexPrefix() + "_" + EsSuffix.GOODS_INDEX_NAME;
    }

    private EsGoodsIndex wrapperEsGoodsIndex(GoodsSkuDTO goodsSku, List<Map<String, Object>> brandList, List<Map<String, Object>> categoryList, List<Map<String, Object>> storeCategoryList) {
        EsGoodsIndex index = new EsGoodsIndex(goodsSku);

        //商品参数索引
        if (CharSequenceUtil.isNotEmpty(goodsSku.getParams())) {
            List<GoodsParamsDTO> goodsParamDTOS = JSONUtil.toList(goodsSku.getParams(), GoodsParamsDTO.class);
            index = new EsGoodsIndex(goodsSku, goodsParamDTOS);
        }
        //商品分类索引
        if (CollUtil.isNotEmpty(categoryList) && CharSequenceUtil.isNotEmpty(goodsSku.getCategoryPath())) {
            StringBuilder categoryNamePath = new StringBuilder();
            categoryList.stream().filter(o -> goodsSku.getCategoryPath().contains(o.get("id").toString())).forEach(p -> categoryNamePath.append(p.get("name")).append(","));
            if (CharSequenceUtil.isNotEmpty(categoryNamePath)) {
                categoryNamePath.deleteCharAt(categoryNamePath.length() - 1);
                index.setCategoryNamePath(categoryNamePath.toString());
            }
        }
        //商品品牌索引
        if (CollUtil.isNotEmpty(brandList) && CharSequenceUtil.isNotEmpty(goodsSku.getBrandId())) {
            Optional<Map<String, Object>> brandInfo = brandList.stream().filter(p -> p.get("id").equals(goodsSku.getBrandId())).findFirst();
            if (brandInfo.isPresent()) {
                index.setBrandName(brandInfo.get().get("name").toString());
                index.setBrandUrl(brandInfo.get().get("logo").toString());
            }
        }
        //店铺分类索引
        if (CollUtil.isNotEmpty(storeCategoryList) && CharSequenceUtil.isNotEmpty(goodsSku.getStoreCategoryPath())) {
            StringBuilder storeCategoryNamePath = new StringBuilder();
            storeCategoryList.stream().filter(o -> goodsSku.getStoreCategoryPath().contains(o.get("id").toString())).forEach(p -> storeCategoryNamePath.append(p.get("label_name").toString()).append(","));
            if (CharSequenceUtil.isNotEmpty(storeCategoryNamePath)) {
                storeCategoryNamePath.deleteCharAt(storeCategoryNamePath.length() - 1);
                index.setStoreCategoryNamePath(storeCategoryNamePath.toString());
            }
        }
        return index;
    }
}
