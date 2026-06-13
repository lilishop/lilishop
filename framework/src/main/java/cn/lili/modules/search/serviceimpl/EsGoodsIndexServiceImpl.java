package cn.lili.modules.search.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.map.MapUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.thread.ThreadUtil;
import cn.hutool.core.util.ReflectUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.GsonUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.elasticsearch.ElasticsearchIndexAbstractService;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.goods.entity.dto.GoodsParamsItemDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsSalesModeEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.*;
import cn.lili.modules.promotion.entity.dos.BasePromotions;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
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
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.client.elc.NativeQueryBuilder;
import org.springframework.data.elasticsearch.core.IndexOperations;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.SearchPage;
import org.springframework.data.elasticsearch.core.document.Document;
import org.springframework.data.elasticsearch.core.query.FetchSourceFilter;
import org.springframework.data.elasticsearch.core.query.Query;
import org.springframework.data.elasticsearch.core.query.UpdateQuery;
import org.springframework.stereotype.Service;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;

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
@ConditionalOnProperty(value = "lili.data.elasticsearch.enabled", havingValue = "true", matchIfMissing = true)
public class EsGoodsIndexServiceImpl extends ElasticsearchIndexAbstractService implements EsGoodsIndexService {

    public static final String PROMOTION_PRICE = "promotionPrice";
    private static final String IGNORE_FIELD = "serialVersionUID,promotionMap,id,goodsId";
    private static final String KEY_SUCCESS = "success";
    private static final String KEY_FAIL = "fail";
    private static final String KEY_PROCESSED = "processed";
    private final Map<String, Field> fieldMap = ReflectUtil.getFieldMap(EsGoodsIndex.class);

    private static final String CURRENT_INFO = "currentInfo";
    @Autowired
    private EsGoodsIndexRepository goodsIndexRepository;
    @Autowired
    private EsGoodsSearchService goodsSearchService;
    @Autowired
    private GoodsService goodsService;
    @Autowired
    @Lazy
    private PromotionService promotionService;

    @Autowired
    @Lazy
    private PromotionGoodsService promotionGoodsService;

    @Autowired
    private CustomWordsService customWordsService;

    @Autowired
    private GoodsSkuService goodsSkuService;
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
            for (int i = 1; ; i++) {

                log.error("第" + i + "页");

                pageVO.setPageSize(1000);
                pageVO.setPageNumber(i);
                pageVO.setNotConvert(true);
                pageVO.setSort("_id");
                pageVO.setOrder("asc");

                NativeQueryBuilder searchQueryBuilder = esGoodsSearchService.createSearchQueryBuilder(goodsSearchParams, pageVO);
                // 仅返回 _source 中的文档字段 "id"，并启用 source 抓取；符合当前 FetchSourceFilter(Boolean, includes, excludes) 签名
                searchQueryBuilder.withSourceFilter(new FetchSourceFilter(true, new String[]{"id"}, null));

                Pageable pageable = PageRequest.of(0, 1000);
                //分页
                searchQueryBuilder.withPageable(pageable);
                Query query = searchQueryBuilder.build();

                SearchPage<EsGoodsIndex> searchHits = goodsSearchService.searchGoods(query, EsGoodsIndex.class);

                if (searchHits == null || searchHits.isEmpty()) {
                    break;
                }

                List<String> idList = searchHits.getContent()
                        .stream()
                        .map(SearchHit::getContent)
                        .map(EsGoodsIndex::getId)
                        .collect(Collectors.toList());

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
                QueryWrapper<GoodsSkuDTO> skuQueryWrapper = new QueryWrapper<>();
                skuQueryWrapper.eq("gs.auth_flag", GoodsAuthEnum.PASS.name());
                skuQueryWrapper.eq("gs.market_enable", GoodsStatusEnum.UPPER.name());
                skuQueryWrapper.eq("gs.delete_flag", false);
                skuIPage = goodsSkuService.getGoodsSkuDTOByPage(PageUtil.initPage(searchParams), skuQueryWrapper);

                if (skuIPage == null || CollUtil.isEmpty(skuIPage.getRecords())) {
                    break;
                }
                for (GoodsSkuDTO goodsSkuDTO : skuIPage.getRecords()) {
                    GoodsSku goodsSku = goodsSkuService.getById(goodsSkuDTO.getId());
                    cache.put(GoodsSkuService.getCacheKeys(goodsSkuDTO.getId()), goodsSku, 600L);
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
                                                (j.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_GOODS_CATEGORY.name()) && "0".equals(j.getStoreId()) && j.getScopeId().contains(goodsSku.getCategoryPath())) ||
                                                (j.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_GOODS_CATEGORY.name()) && j.getStoreId().equals(goodsSku.getStoreId()) && j.getScopeId().contains(goodsSku.getCategoryPath()))
                                )
                                .collect(Collectors.toList());
                        if (CollUtil.isNotEmpty(promotionGoods)) {
                            esGoodsIndex.setPromotionMapJson(JSON.toJSONString(promotionService.wrapperPromotionMapList(promotionGoods)));
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
    public <T> T findIndexById(String id, Class<T> clazz) {
        T index = this.findById(id, clazz);
        if (index == null) {
            log.error("Id为{}的索引不存在！", id);
            return null;
        }
        return index;
    }

    @Override
    public EsGoodsIndex findEsGoodsIndexById(String id) {
        GoodsSku goodsSku = goodsSkuService.getById(id);
        if (goodsSku == null) {
            log.error("商品skuId为{}不存在！", id);
            return null;
        }

        EsGoodsIndex goodsIndex = this.findIndexById(id, EsGoodsIndex.class);

        if (goodsIndex == null) {
            goodsIndex = this.getResetEsGoodsIndex(goodsSku);
        }
        return goodsIndex;
    }

    @Override
    public EsGoodsIndex addIndex(EsGoodsIndex goodsIndex) {
        try {

            //分词器分词
//            this.analyzeAndSaveWords(goods);
            cache.remove(CachePrefix.GOODS_SKU.getPrefix(goodsIndex.getId()));
            // 生成索引
            return this.saveIndex(goodsIndex);
        } catch (Exception e) {
            log.error("为商品[{}]生成索引异常", goodsIndex.getGoodsName(), e);
            return null;
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

    @Override
    public void updateIndex(Map<String, Object> queryFields, Map<String, Object> updateFields) {
// 如果queryFields存在id，则删除缓存
        if (queryFields != null && queryFields.containsKey("id")) {
            String id = queryFields.get("id").toString();
            if (JSON.isValidArray(id)) {
                JSON.parseArray(id).forEach(item -> cache.remove(CachePrefix.GOODS_SKU.getPrefix(item.toString())));
            }
        }
        updateIndexFields(queryFields, updateFields, EsGoodsIndex.class);
    }

    /**
     * 更新商品索引的的部分属性（只填写更新的字段，不需要更新的字段不要填写）
     *
     * @param goods
     */
    @Override
    public void updateIndex(EsGoodsIndex goods) {
        this.addIndex(goods);
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
        deleteIndexByIds(ids, EsGoodsIndex.class);
    }

    @Override
    public void initIndex(List<EsGoodsIndex> goodsIndexList, boolean regeneratorIndex) {
        String flagCacheKey = CachePrefix.INIT_INDEX_FLAG.getPrefix();
        String processKey = CachePrefix.INIT_INDEX_PROCESS.getPrefix();


        if (goodsIndexList == null || goodsIndexList.isEmpty()) {
            //初始化标识
            cache.put(processKey, null);
            cache.put(flagCacheKey, false);
            return;
        }
        //预校验
        recheck(regeneratorIndex);

        Map<String, Object> result = (Map<String, Object>) cache.get(processKey);
        if (!goodsIndexList.isEmpty()) {
            for (EsGoodsIndex goodsIndex : goodsIndexList) {
                try {
                    result.put(CURRENT_INFO, goodsIndex.getGoodsName());
                    log.debug("生成商品索引：{}", goodsIndex);
                    this.addIndex(goodsIndex);
                    result.put(KEY_SUCCESS, Long.parseLong(result.get(KEY_SUCCESS).toString()) + 1);
                } catch (Exception e) {
                    log.error("商品{}生成索引错误！", goodsIndex);
                    result.put(KEY_FAIL, Long.parseLong(result.get(KEY_FAIL).toString()) + 1);
                }
                result.put(KEY_PROCESSED, Long.parseLong(result.get(KEY_PROCESSED).toString()) + 1);
                cache.put(processKey, result);
            }
        }
        cache.put(processKey, result);
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
        List<UpdateQuery> updateQueries = new ArrayList<>();
        log.debug("更新商品索引的促销信息----------");
        log.debug("商品ids: {}", ids);
        log.debug("活动: {}", promotion);
        log.debug("key: {}", key);
        Class<?> clazz = null;
        for (String id : ids) {
            EsGoodsIndex goodsIndex = this.findIndexById(id, EsGoodsIndex.class);
            PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
            searchParams.setPromotionId(promotion.getId());
            searchParams.setSkuId(id);
            PromotionGoods promotionsGoods = promotionGoodsService.getPromotionsGoods(searchParams);
            clazz = addUpdateQuery(promotion, key, updateQueries, clazz, goodsIndex);
            // 如果促销活动为秒杀或者拼团设置促销价格
            if (promotionsGoods != null && (PromotionTypeEnum.SECKILL.name().equals(promotionsGoods.getPromotionType()) || PromotionTypeEnum.PINTUAN.name().equals(promotionsGoods.getPromotionType()))) {
                updateQueries.add(UpdateQuery.builder(id).withDocument(Document.from(MapUtil.builder(PROMOTION_PRICE, promotionsGoods.getPrice()).build())).build());
            }

        }
        if (updateQueries.isEmpty()) {
            return;
        }
        this.client.bulkUpdate(updateQueries, clazz);
    }


    @Override
    public void updateEsGoodsIndexByList(List<PromotionGoods> promotionGoodsList, BasePromotions promotion, String key) {
        if (promotionGoodsList == null || promotionGoodsList.isEmpty()) {
            return;
        }

        List<UpdateQuery> updateQueries = new ArrayList<>();
        Class<?> clazz = null;

        for (PromotionGoods promotionGoods : promotionGoodsList) {
            // 设置促销活动的开始和结束时间
            if (promotion.getStartTime() == null || promotion instanceof Seckill) {
                promotion.setStartTime(promotionGoods.getStartTime());
            }
            if (promotion.getEndTime() == null || promotion instanceof Seckill) {
                promotion.setEndTime(promotionGoods.getEndTime());
            }

            // 查找商品索引
            EsGoodsIndex goodsIndex = findEsGoodsIndexById(promotionGoods.getSkuId());
            clazz = addUpdateQuery(promotion, key, updateQueries, clazz, goodsIndex);

            // 如果促销活动为秒杀、满减或拼团，设置促销价格
            if (goodsIndex != null &&
                    (PromotionTypeEnum.SECKILL.name().equals(promotionGoods.getPromotionType()) ||
                            PromotionTypeEnum.PINTUAN.name().equals(promotionGoods.getPromotionType()))) {
                updateQueries.add(UpdateQuery.builder(goodsIndex.getId())
                        .withDocument(Document.from(MapUtil.builder(PROMOTION_PRICE, promotionGoods.getPrice()).build()))
                        .build());
            }
        }

        // 批量更新索引
        if (!updateQueries.isEmpty()) {
            this.client.bulkUpdate(updateQueries, clazz);
        }
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

    /**
     * 删除索引中指定的促销活动id的促销活动
     *
     * @param promotionsKey 促销活动Key
     */
    @Override
    public void deleteEsGoodsPromotionByPromotionKey(List<String> skuIds, String promotionsKey) {
        List<UpdateQuery> updateQueries = new ArrayList<>();
        log.debug("删除商品活动索引");
        log.debug("商品skuIds: {}", skuIds);
        log.debug("活动Key: {}", promotionsKey);
        if (skuIds == null || skuIds.isEmpty()) {
            return;
        }
        Class<?> clazz = null;
        for (String skuId : skuIds) {
            EsGoodsIndex goodsIndex = this.findIndexById(skuId, EsGoodsIndex.class);
            //商品索引不为空
            if (goodsIndex != null) {
                UpdateQuery updateRequest = this.removePromotionByPromotionKey(goodsIndex, promotionsKey);
                if (updateRequest != null) {
                    updateQueries.add(updateRequest);
                    clazz = goodsIndex.getClass();
                    cache.remove(CachePrefix.GOODS_SKU.getPrefix(goodsIndex.getId()));
                }
            } else {
                log.error("部分商品促销信息失败！skuId 为 【{}】的索引不存在！", skuId);
            }
        }
        if (updateQueries.isEmpty()) {
            return;
        }
        this.client.bulkUpdate(updateQueries, clazz);
    }

    @Override
    public void deleteEsGoodsPromotionByPromotionKey(String promotionsKey) {

    }


    /**
     * 从索引中删除指定促销活动id的促销活动
     *
     * @param goodsIndex    索引
     * @param promotionsKey 促销活动key
     */
    private UpdateQuery removePromotionByPromotionKey(EsGoodsIndex goodsIndex, String promotionsKey) {
        Map<String, Object> promotionMap = goodsIndex.getOriginPromotionMap();
        if (promotionMap != null && !promotionMap.isEmpty()) {
            //如果存在同促销ID的活动删除
            Map<String, Object> filterPromotionMap =
                    promotionMap.entrySet().stream().filter(i -> !i.getKey().equals(promotionsKey)).collect(Collectors.toMap(Map.Entry::getKey,
                            Map.Entry::getValue));
            return this.getGoodsIndexPromotionUpdateRequest(goodsIndex, filterPromotionMap);
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
                        BasePromotions promotion = JSON.parseObject(JSON.toJSONString(j.getValue()), BasePromotions.class);
                        return promotion.getEndTime() != null && promotion.getEndTime().getTime() < DateUtil.date().getTime();
                    });

                    // 更新回 goodsIndex 对象
                    goodsIndex.setPromotionMapJson(JSON.toJSONString(promotionMap));
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
     * @param goodsSku 商品sku信息
     * @return 商品索引
     */
    @Override
    public EsGoodsIndex getResetEsGoodsIndex(GoodsSku goodsSku) {
        // 检查商品是否已删除
        if (Boolean.TRUE.equals(goodsSku.getDeleteFlag())) {
            return null;
        }

        // 获取商品信息
        Goods goods = goodsService.getById(goodsSku.getGoodsId());
        if (goods == null) {
            return null;
        }

        // 构建商品SKU DTO
        GoodsSkuDTO goodsSkuDTO = new GoodsSkuDTO(goodsSku, JSON.parseArray(goods.getParams(), GoodsParamsDTO.class));

        // 初始化品牌、分类和店铺分类路径
        List<String> brandIds = new ArrayList<>();
        List<String> categoryPaths = new ArrayList<>();
        List<String> storeCategoryPaths = new ArrayList<>();
        packageAttribute(goodsSkuDTO, brandIds, categoryPaths, storeCategoryPaths);

        // 获取品牌、分类和店铺分类信息
        List<Map<String, Object>> brandList = CollUtil.isNotEmpty(brandIds)
                ? brandService.getBrandsMapsByCategory(CollUtil.distinct(brandIds), "id,name,logo")
                : new ArrayList<>();
        List<Map<String, Object>> categoryList = CollUtil.isNotEmpty(categoryPaths)
                ? categoryService.listMapsByIdsOrderByLevel(CollUtil.distinct(categoryPaths), "id,name")
                : new ArrayList<>();
        List<Map<String, Object>> storeCategoryList = CollUtil.isNotEmpty(storeCategoryPaths)
                ? storeGoodsLabelService.listMapsByStoreIds(CollUtil.distinct(storeCategoryPaths), "id,label_name")
                : new ArrayList<>();

        // 构建商品索引
        EsGoodsIndex index = wrapperEsGoodsIndex(goodsSkuDTO, brandList, categoryList, storeCategoryList);


        // 设置促销信息
        try {
            List<PromotionGoods> skuValidPromotions = promotionGoodsService.findSkuValidPromotions(Collections.singletonList(goodsSkuDTO.getId()));
            List<PromotionGoods> promotionGoods = skuValidPromotions.stream()
                    .filter(j -> CharSequenceUtil.isNotEmpty(j.getSkuId()) && j.getSkuId().equals(goodsSku.getId())
                            || j.getScopeType().equals(PromotionsScopeTypeEnum.ALL.name()) && j.getStoreId().equals("0")
                            || j.getScopeType().equals(PromotionsScopeTypeEnum.ALL.name()) && j.getStoreId().equals(goodsSku.getStoreId())
                            || j.getScopeType().equals(PromotionsScopeTypeEnum.PORTION_GOODS_CATEGORY.name()) && j.getScopeId().contains(goodsSku.getCategoryPath()))
                    .toList();
            setIndexPromotionsInfo(promotionGoods, index);
        } catch (Exception e) {
            log.error("获取商品促销信息失败！", e);
        }

        // 添加索引并返回
        return addIndex(index);
    }

    /**
     * 修改商品活动索引
     *
     * @param goodsIndex 商品索引
     * @param key        关键字
     * @param promotion  活动
     */
    private UpdateQuery updateGoodsIndexPromotion(EsGoodsIndex goodsIndex, String key, BasePromotions promotion) {
        // 原始促销映射，使用防御性拷贝避免直接修改原对象（减少潜在副作用）
        Map<String, Object> originPromotionMap = goodsIndex.getOriginPromotionMap();
        Map<String, Object> promotionMap = (originPromotionMap == null || originPromotionMap.isEmpty())
                ? new HashMap<>(1)
                : new HashMap<>(originPromotionMap.size() + 1); // 预估容量，减少扩容

        if (originPromotionMap != null && !originPromotionMap.isEmpty()) {
            promotionMap.putAll(originPromotionMap);
        }

        // 结束/关闭活动则移除；否则更新指定key
        String status = promotion.getPromotionStatus();
        boolean ended = PromotionsStatusEnum.END.name().equals(status) || PromotionsStatusEnum.CLOSE.name().equals(status);
        if (ended) {
            promotionMap.remove(key);
        } else {
            // 删除促销活动里的 PromotionGoodsList，避免索引冗余
            JSONObject promotionJson = JSON.parseObject(JSON.toJSONString(promotion));
            promotionJson.remove("promotionGoodsList");
            promotionMap.put(key, promotionJson);
        }

        return this.getGoodsIndexPromotionUpdateRequest(goodsIndex, promotionMap);
    }

    /**
     * 以更新部分字段的方式更新索引促销信息
     * <p>
     * 说明：
     * - 将促销信息 Map 序列化为 JSON 字符串存入字段 `promotionMapJson`，避免 ES 动态映射造成类型冲突。
     * - 使用 inline 脚本（默认 painless）在服务器端执行字段赋值，适合进行局部更新而不回传整文档。
     * - 设置 `retryOnConflict(3)` 以在版本冲突（并发写）时自动重试，提升更新成功率。
     *
     * @param goodsIndex   商品索引实体（用于获取文档 id）
     * @param promotionMap 促销信息 Map（可能为 null 或空）
     * @return UpdateQuery 供批量或单次更新调用
     */
    private UpdateQuery getGoodsIndexPromotionUpdateRequest(EsGoodsIndex goodsIndex, Map<String, Object> promotionMap) {
        // 1) 保护性处理：转换为非空 Map，再序列化为 String
        final Map<String, Object> safeMap = (promotionMap == null || promotionMap.isEmpty())
                ? Collections.emptyMap()
                : promotionMap;
        final String promotionMapJson = GsonUtils.toJson(safeMap);

        // 2) 使用单元素不可变 Map 作为脚本参数，降低分配和误改风险
        final Map<String, Object> params = Collections.singletonMap("promotionMap", promotionMapJson);

        // 3) 构造脚本更新请求：将 JSON 字符串写入 _source.promotionMapJson
        return UpdateQuery.builder(goodsIndex.getId())
                .withParams(params)
                .withScript("ctx._source.promotionMapJson = params.promotionMap;")
                .withRetryOnConflict(3)
                .build();
    }


    /**
     * 构建商品的 ES 索引对象
     * <p>
     * 说明：
     * - 优先从 SKU 的参数 JSON 中构建带参数的索引；若参数为空或解析失败，则回退为基础索引。
     * - 分类、品牌、店铺分类信息通过外部传入的列表进行匹配并填充到索引中。
     * - 对可能为空的字段做保护性判断，避免 NPE。
     *
     * @param goodsSku          商品 SKU 聚合信息
     * @param brandList         品牌信息列表（元素包含 id、name、logo）
     * @param categoryList      平台分类列表（元素包含 id、name）
     * @param storeCategoryList 店铺分类列表（元素包含 id、label_name）
     * @return 填充后的商品索引
     */
    private EsGoodsIndex wrapperEsGoodsIndex(GoodsSkuDTO goodsSku,
                                             List<Map<String, Object>> brandList,
                                             List<Map<String, Object>> categoryList,
                                             List<Map<String, Object>> storeCategoryList) {
        // 1) 优先尝试根据参数 JSON 构建索引；解析失败时回退为基础索引
        EsGoodsIndex index;
        if (CharSequenceUtil.isNotEmpty(goodsSku.getParams())) {
            try {
                List<GoodsParamsItemDTO> goodsParamDTOS = JSON.parseArray(goodsSku.getParams(), GoodsParamsItemDTO.class);
                index = CollUtil.isNotEmpty(goodsParamDTOS) ? new EsGoodsIndex(goodsSku, goodsParamDTOS) : new EsGoodsIndex(goodsSku);
            } catch (Exception parseEx) {
                // 参数 JSON 非预期时，回退为基础索引，避免构建中断
                index = new EsGoodsIndex(goodsSku);
                log.warn("SKU 参数解析失败，使用基础索引。skuId={}, params={}", goodsSku.getId(), goodsSku.getParams(), parseEx);
            }
        } else {
            index = new EsGoodsIndex(goodsSku);
        }

        // 2) 平台分类路径名称：根据 categoryPath 匹配分类 id，拼接分类名称
        if (CollUtil.isNotEmpty(categoryList) && CharSequenceUtil.isNotEmpty(goodsSku.getCategoryPath())) {
            String categoryNamePath = categoryList.stream()
                    .filter(o -> goodsSku.getCategoryPath().contains(String.valueOf(o.get("id"))))
                    .map(p -> String.valueOf(p.get("name")))
                    .filter(CharSequenceUtil::isNotEmpty)
                    .collect(Collectors.joining(","));
            if (CharSequenceUtil.isNotEmpty(categoryNamePath)) {
                index.setCategoryNamePath(categoryNamePath);
            }
        }

        // 3) 品牌信息：按品牌 id 匹配，填充品牌名称与 logo
        if (CollUtil.isNotEmpty(brandList) && CharSequenceUtil.isNotEmpty(goodsSku.getBrandId())) {
            Optional<Map<String, Object>> brandInfo = brandList.stream()
                    .filter(p -> goodsSku.getBrandId().equals(String.valueOf(p.get("id"))))
                    .findFirst();
            if (brandInfo.isPresent()) {
                Map<String, Object> b = brandInfo.get();
                String brandName = String.valueOf(b.get("name"));
                String brandLogo = String.valueOf(b.get("logo"));
                if (CharSequenceUtil.isNotEmpty(brandName)) {
                    index.setBrandName(brandName);
                }
                if (CharSequenceUtil.isNotEmpty(brandLogo)) {
                    index.setBrandUrl(brandLogo);
                }
            }
        }

        // 4) 店铺分类路径名称：根据店铺分类 path 匹配 id，拼接分类名称
        if (CollUtil.isNotEmpty(storeCategoryList) && CharSequenceUtil.isNotEmpty(goodsSku.getStoreCategoryPath())) {
            String storeCategoryNamePath = storeCategoryList.stream()
                    .filter(o -> goodsSku.getStoreCategoryPath().contains(String.valueOf(o.get("id"))))
                    .map(p -> String.valueOf(p.get("label_name")))
                    .filter(CharSequenceUtil::isNotEmpty)
                    .collect(Collectors.joining(","));
            if (CharSequenceUtil.isNotEmpty(storeCategoryNamePath)) {
                index.setStoreCategoryNamePath(storeCategoryNamePath);
            }
        }

        return index;
    }

    /**
     * 设置索引中的促销信息
     * <p>
     * 说明：
     * - 将有效的促销活动列表转换为 JSON 写入 `promotionMapJson`，避免 ES 动态映射冲突。
     * - 当存在秒杀或拼团活动时，设置促销价格（取首个匹配活动的价格）。
     * - 做空值保护与容错处理，减少对索引构建流程的影响。
     *
     * @param promotionGoods 有效促销活动列表
     * @param index          商品索引对象
     */
    private void setIndexPromotionsInfo(List<PromotionGoods> promotionGoods, EsGoodsIndex index) {
        if (CollUtil.isEmpty(promotionGoods) || index == null) {
            return;
        }

        // 写入促销信息 JSON（服务端封装结构），避免索引映射漂移
        try {
            index.setPromotionMapJson(GsonUtils.toJson(promotionService.wrapperPromotionMapList(promotionGoods)));
        } catch (Exception e) {
            // 容错：当序列化异常时不阻断构建流程
            log.warn("封装促销信息失败，跳过写入 promotionMapJson。skuId={}, error={}", index.getId(), e.getMessage());
        }

        // 提取秒杀/拼团的促销价格（单次过滤取首个）
        final String SECKILL = PromotionTypeEnum.SECKILL.name();
        final String PINTUAN = PromotionTypeEnum.PINTUAN.name();
        Double promotionPrice = promotionGoods.stream()
                .filter(pg -> SECKILL.equals(pg.getPromotionType()) || PINTUAN.equals(pg.getPromotionType()))
                .map(PromotionGoods::getPrice)
                .findFirst()
                .orElse(null);

        if (promotionPrice != null) {
            index.setPromotionPrice(promotionPrice);
        }
    }

    private void packageAttribute(GoodsSkuDTO goodsSkuDTO, List<String> brandIds, List<String> categoryPaths, List<String> storeCategoryPaths) {
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

    /**
     * 更新商品索引的促销信息
     *
     * @param promotion     促销信息
     * @param key           促销信息的key
     * @param updateQueries 更新请求
     * @param clazz         类
     * @param goodsIndex    商品索引
     * @return 商品索引类
     */
    private Class<?> addUpdateQuery(BasePromotions promotion, String key, List<UpdateQuery> updateQueries, Class<?> clazz, EsGoodsIndex goodsIndex) {
        if (goodsIndex != null) {
            //更新索引
            updateQueries.add(this.updateGoodsIndexPromotion(goodsIndex, key, promotion));
            clazz = goodsIndex.getClass();
            cache.remove(CachePrefix.GOODS_SKU.getPrefix(goodsIndex.getId()));
        }
        return clazz;
    }

    /**
     * 预校验，不存在则删除
     */
    private void recheck(boolean regeneratorIndex) {

        if (!regeneratorIndex) {
            return;
        }
        IndexOperations indexOps = this.client.indexOps(EsGoodsIndex.class);
        recreateIndex(indexOps);
    }

    private void recreateIndex(IndexOperations indexOps) {
        if (!indexOps.exists()) {
            indexOps.createWithMapping();
        } else {
            indexOps.delete();
            indexOps.createWithMapping();
        }
    }

}
