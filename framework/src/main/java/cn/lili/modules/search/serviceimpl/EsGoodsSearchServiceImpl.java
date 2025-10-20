package cn.lili.modules.search.serviceimpl;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ArrayUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.entity.dos.EsGoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import cn.lili.modules.search.entity.dto.ParamOptions;
import cn.lili.modules.search.entity.dto.SelectorOptions;
import cn.lili.modules.search.service.EsGoodsSearchService;
import cn.lili.modules.search.utils.SqlFilter;
import com.alibaba.druid.util.StringUtils;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import lombok.extern.slf4j.Slf4j;
import org.apache.lucene.search.join.ScoreMode;
import org.elasticsearch.common.lucene.search.function.FieldValueFactorFunction;
import org.elasticsearch.common.lucene.search.function.FunctionScoreQuery;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.MatchQueryBuilder;
import org.elasticsearch.index.query.Operator;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.index.query.functionscore.FieldValueFactorFunctionBuilder;
import org.elasticsearch.index.query.functionscore.FunctionScoreQueryBuilder;
import org.elasticsearch.index.query.functionscore.ScoreFunctionBuilders;
import org.elasticsearch.search.aggregations.*;
import org.elasticsearch.search.aggregations.bucket.nested.ParsedNested;
import org.elasticsearch.search.aggregations.bucket.terms.ParsedStringTerms;
import org.elasticsearch.search.aggregations.bucket.terms.Terms;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.core.*;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.elasticsearch.core.query.Query;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * ES商品搜索业务层实现
 *
 * @author paulG
 * @since 2020/10/16
 **/
@Slf4j
@Service
public class EsGoodsSearchServiceImpl implements EsGoodsSearchService {

    // 最小分词匹配
    private static final String MINIMUM_SHOULD_MATCH = "20%";

    private static final String ATTR_PATH = "attrList";
    private static final String ATTR_VALUE = "attrList.value";
    private static final String ATTR_NAME = "attrList.name";
    private static final String ATTR_SORT = "attrList.sort";
    private static final String ATTR_BRAND_ID = "brandId";
    private static final String ATTR_BRAND_NAME = "brandNameAgg";
    private static final String ATTR_BRAND_URL = "brandUrlAgg";
    private static final String ATTR_NAME_KEY = "nameList";
    private static final String ATTR_VALUE_KEY = "valueList";
    // 限制 terms 聚合 size，防止内存压力
    private static final int MAX_AGG_SIZE = 200;
    /**
     * ES
     */
    @Autowired
    private ElasticsearchOperations restTemplate;
    /**
     * 缓存
     */
    @Autowired
    private Cache<Object> cache;

    @Override
    public SearchPage<EsGoodsIndex> searchGoods(EsGoodsSearchDTO searchDTO, PageVO pageVo) {

        //如果搜索词不为空，且明显不是sql注入，那么就将搜索词加入热搜词
        //PS:线上环境运行很多客户反馈被sql攻击，写在了搜索热词里，这里控制命中关键字就不做热词统计，如果线上比较严格可以调用关键词替换，不过不建议这么做
        if (CharSequenceUtil.isNotBlank(searchDTO.getKeyword()) && Boolean.FALSE.equals(SqlFilter.hit(searchDTO.getKeyword()))) {
            cache.incrementScore(CachePrefix.HOT_WORD.getPrefix(), searchDTO.getKeyword());
        }
        NativeSearchQueryBuilder searchQueryBuilder = createSearchQueryBuilder(searchDTO, pageVo);
//        searchQueryBuilder.withCollapseField("goodsId.keyword");
        NativeSearchQuery searchQuery = searchQueryBuilder.build();
        searchQuery.setTrackTotalHits(true);
        log.debug("searchGoods DSL:{}", searchQuery.getQuery());
        SearchHits<EsGoodsIndex> search = restTemplate.search(searchQuery, EsGoodsIndex.class);
        return SearchHitSupport.searchPageFor(search, searchQuery.getPageable());
    }

    @Override
    public <T> SearchPage<T> searchGoods(Query searchQuery, Class<T> clazz) {
        SearchHits<T> search = restTemplate.search(searchQuery, clazz);
        return SearchHitSupport.searchPageFor(search, searchQuery.getPageable());
    }

    @Override
    public Page<EsGoodsIndex> searchGoodsByPage(EsGoodsSearchDTO searchDTO, PageVO pageVo) {
        // 判断商品索引是否存在
        if (!restTemplate.indexOps(EsGoodsIndex.class).exists()) {
            return null;
        }

        SearchPage<EsGoodsIndex> esGoodsIndices = this.searchGoods(searchDTO, pageVo);
        Page<EsGoodsIndex> resultPage = new Page<>();
        if (esGoodsIndices != null && !esGoodsIndices.getContent().isEmpty()) {
            List<EsGoodsIndex> collect = esGoodsIndices.getSearchHits().getSearchHits().stream().map(SearchHit::getContent).collect(Collectors.toList());
            resultPage.setRecords(collect);
            resultPage.setPages(esGoodsIndices.getTotalPages());
            resultPage.setCurrent(esGoodsIndices.getNumber() + 1L);
            resultPage.setSize(esGoodsIndices.getSize());
            resultPage.setTotal(esGoodsIndices.getTotalElements());
        }
        return resultPage;
    }

    @Override
    public EsGoodsRelatedInfo getSelector(EsGoodsSearchDTO goodsSearch, PageVO pageVo) {
        // 判断商品索引是否存在
        if (!restTemplate.indexOps(EsGoodsIndex.class).exists()) {
            return null;
        }
        NativeSearchQueryBuilder builder = createSearchQueryBuilder(goodsSearch, null);
        //分类
        AggregationBuilder categoryNameBuilder = AggregationBuilders.terms("categoryNameAgg").field("categoryNamePath.keyword").size(MAX_AGG_SIZE);
        builder.addAggregation(AggregationBuilders.terms("categoryAgg").field("categoryPath").size(MAX_AGG_SIZE).subAggregation(categoryNameBuilder));

        //品牌
        AggregationBuilder brandNameBuilder = AggregationBuilders.terms(ATTR_BRAND_NAME).field("brandName.keyword").size(1);
        builder.addAggregation(AggregationBuilders.terms("brandIdNameAgg").field(ATTR_BRAND_ID).size(MAX_AGG_SIZE).subAggregation(brandNameBuilder));

        AggregationBuilder brandUrlBuilder = AggregationBuilders.terms(ATTR_BRAND_URL).field("brandUrl.keyword").size(1);
        builder.addAggregation(AggregationBuilders.terms("brandIdUrlAgg").field(ATTR_BRAND_ID).size(MAX_AGG_SIZE).subAggregation(brandUrlBuilder));
        //参数
        AggregationBuilder valuesBuilder = AggregationBuilders.terms("valueAgg").field(ATTR_VALUE).size(MAX_AGG_SIZE);
        AggregationBuilder sortBuilder = AggregationBuilders.sum("sortAgg").field(ATTR_SORT);
        AggregationBuilder paramsNameBuilder = AggregationBuilders.terms("nameAgg").field(ATTR_NAME).size(MAX_AGG_SIZE)
                .subAggregation(sortBuilder).order(BucketOrder.aggregation("sortAgg", false)).subAggregation(valuesBuilder);
        builder.addAggregation(AggregationBuilders.nested("attrAgg", ATTR_PATH).subAggregation(paramsNameBuilder));

        NativeSearchQuery searchQuery = builder.build();
        searchQuery.setMaxResults(0);
        SearchHits<EsGoodsIndex> search = restTemplate.search(searchQuery, EsGoodsIndex.class);

        log.debug("getSelector DSL:{}", searchQuery.getQuery());
        Map<String, Aggregation> aggregationMap = Objects.requireNonNull(search.getAggregations()).getAsMap();
        return convertToEsGoodsRelatedInfo(aggregationMap, goodsSearch);
    }

    @Override
    public List<EsGoodsIndex> getEsGoodsBySkuIds(List<String> skuIds, PageVO pageVo) {
        NativeSearchQueryBuilder searchQueryBuilder = new NativeSearchQueryBuilder();
        if (pageVo != null) {
            int pageNumber = pageVo.getPageNumber() - 1;
            if (pageNumber < 0) {
                pageNumber = 0;
            }
            Pageable pageable = PageRequest.of(pageNumber, pageVo.getPageSize());
            //分页
            searchQueryBuilder.withPageable(pageable);
        }
        NativeSearchQuery build = searchQueryBuilder.build();
        build.setIds(skuIds);
        return restTemplate.multiGet(build, EsGoodsIndex.class, restTemplate.getIndexCoordinatesFor(EsGoodsIndex.class));
    }

    /**
     * 根据id获取商品索引
     *
     * @param id 商品skuId
     * @return 商品索引
     */
    @Override
    public EsGoodsIndex getEsGoodsById(String id) {
        return this.restTemplate.get(id, EsGoodsIndex.class);
    }

    /**
     * 转换搜索结果为聚合商品展示信息
     *
     * @param aggregationMap 搜索结果
     * @return 聚合商品展示信息
     */
    private EsGoodsRelatedInfo convertToEsGoodsRelatedInfo(Map<String, Aggregation> aggregationMap, EsGoodsSearchDTO goodsSearch) {
        EsGoodsRelatedInfo esGoodsRelatedInfo = new EsGoodsRelatedInfo();
        //分类
        List<SelectorOptions> categoryOptions = new ArrayList<>();
        ParsedStringTerms categoryTerms = (ParsedStringTerms) aggregationMap.get("categoryAgg");
        List<? extends Terms.Bucket> categoryBuckets = categoryTerms.getBuckets();
        if (categoryBuckets != null && !categoryBuckets.isEmpty()) {
            categoryOptions = this.convertCategoryOptions(categoryBuckets);
        }
        esGoodsRelatedInfo.setCategories(categoryOptions);

        //品牌
        ParsedStringTerms brandNameTerms = (ParsedStringTerms) aggregationMap.get("brandIdNameAgg");
        ParsedStringTerms brandUrlTerms = (ParsedStringTerms) aggregationMap.get("brandIdUrlAgg");
        List<? extends Terms.Bucket> brandBuckets = brandNameTerms.getBuckets();
        List<? extends Terms.Bucket> brandUrlBuckets = brandUrlTerms.getBuckets();
        List<SelectorOptions> brandOptions = new ArrayList<>();
        if (brandBuckets != null && !brandBuckets.isEmpty()) {
            brandOptions = this.convertBrandOptions(goodsSearch, brandBuckets, brandUrlBuckets);
        }
        esGoodsRelatedInfo.setBrands(brandOptions);

        //参数
        ParsedNested attrTerms = (ParsedNested) aggregationMap.get("attrAgg");
        if (!goodsSearch.getNotShowCol().isEmpty()) {
            if (goodsSearch.getNotShowCol().containsKey(ATTR_NAME_KEY) && goodsSearch.getNotShowCol().containsKey(ATTR_VALUE_KEY)) {
                esGoodsRelatedInfo.setParamOptions(buildGoodsParam(attrTerms, goodsSearch.getNotShowCol().get(ATTR_NAME_KEY)));
            }
        } else {
            esGoodsRelatedInfo.setParamOptions(buildGoodsParam(attrTerms, null));
        }

        return esGoodsRelatedInfo;
    }

    /**
     * 将品牌聚合结果转换品牌选择项
     *
     * @param goodsSearch     查询参数
     * @param brandBuckets    品牌聚合结果桶
     * @param brandUrlBuckets 品牌地址聚合结果桶
     * @return 品牌选择项列表
     */
    private List<SelectorOptions> convertBrandOptions(EsGoodsSearchDTO goodsSearch, List<? extends Terms.Bucket> brandBuckets, List<? extends Terms.Bucket> brandUrlBuckets) {
        List<SelectorOptions> brandOptions = new ArrayList<>();
        // 以 brandId 为 key 构建 URL 桶索引，避免按下标对齐产生越界/错位
        Map<String, Terms.Bucket> brandUrlBucketMap = new HashMap<>();
        if (brandUrlBuckets != null) {
            for (Terms.Bucket urlBucket : brandUrlBuckets) {
                if (urlBucket != null && urlBucket.getKey() != null) {
                    brandUrlBucketMap.put(urlBucket.getKey().toString(), urlBucket);
                }
            }
        }

        Set<String> selectedBrandIds = new HashSet<>();
        if (CharSequenceUtil.isNotEmpty(goodsSearch.getBrandId())) {
            selectedBrandIds.addAll(Arrays.asList(goodsSearch.getBrandId().split("@")));
        }

        for (Terms.Bucket bucket : brandBuckets) {
            if (bucket == null || bucket.getKey() == null) {
                continue;
            }
            String brandId = bucket.getKey().toString();
            // 排除无品牌或已选择品牌
            if ("0".equals(brandId) || (!selectedBrandIds.isEmpty() && selectedBrandIds.contains(brandId))) {
                continue;
            }

            // 品牌名
            String brandName = "";
            Aggregations nameAggs = bucket.getAggregations();
            if (nameAggs != null) {
                Aggregation nameAgg = nameAggs.get(ATTR_BRAND_NAME);
                if (nameAgg instanceof ParsedStringTerms) {
                    brandName = this.getAggregationsBrandOptions((ParsedStringTerms) nameAgg);
                }
            }
            if (StringUtils.isEmpty(brandName)) {
                continue;
            }

            // 品牌 URL
            String brandUrl = "";
            Terms.Bucket urlBucket = brandUrlBucketMap.get(brandId);
            if (urlBucket != null && urlBucket.getAggregations() != null) {
                Aggregation urlAgg = urlBucket.getAggregations().get(ATTR_BRAND_URL);
                if (urlAgg instanceof ParsedStringTerms) {
                    brandUrl = this.getAggregationsBrandOptions((ParsedStringTerms) urlAgg);
                }
            }
            if (StringUtils.isEmpty(brandUrl)) {
                continue;
            }

            SelectorOptions so = new SelectorOptions();
            so.setName(brandName);
            so.setValue(brandId);
            so.setUrl(brandUrl);
            brandOptions.add(so);
        }
        return brandOptions;
    }

    /**
     * 获取品牌聚合结果内的参数
     *
     * @param brandAgg 品牌聚合结果
     * @return 品牌聚合结果内的参数
     */
    private String getAggregationsBrandOptions(ParsedStringTerms brandAgg) {
        List<? extends Terms.Bucket> brandAggBuckets = brandAgg.getBuckets();
        if (brandAggBuckets != null && !brandAggBuckets.isEmpty()) {
            return brandAggBuckets.get(0).getKey().toString();
        }
        return "";
    }


    /**
     * 将分类聚合结果转换分类选择项
     *
     * @param categoryBuckets 分类聚合结果
     * @return 分类选择项集合
     */
    private List<SelectorOptions> convertCategoryOptions(List<? extends Terms.Bucket> categoryBuckets) {
        List<SelectorOptions> categoryOptions = new ArrayList<>();
        for (Terms.Bucket categoryBucket : categoryBuckets) {
            String categoryPath = categoryBucket.getKey().toString();
            ParsedStringTerms categoryNameAgg = categoryBucket.getAggregations().get("categoryNameAgg");
            List<? extends Terms.Bucket> categoryNameBuckets = categoryNameAgg.getBuckets();


            if (!categoryNameBuckets.isEmpty()) {
                String categoryNamePath = categoryNameBuckets.get(0).getKey().toString();
                String[] split = ArrayUtil.distinct(categoryPath.split(","));
                String[] nameSplit = categoryNamePath.split(",");
                if (split.length == nameSplit.length) {
                    for (int i = 0; i < split.length; i++) {
                        SelectorOptions so = new SelectorOptions();
                        so.setName(nameSplit[i]);
                        so.setValue(split[i]);
                        if (!categoryOptions.contains(so)) {
                            categoryOptions.add(so);
                        }
                    }
                }
            }

        }
        return categoryOptions;
    }

    /**
     * 构建商品参数信息
     *
     * @param attrTerms 商品参数搜索结果
     * @param nameList  查询的规格名
     * @return 商品参数信息
     */
    private List<ParamOptions> buildGoodsParam(ParsedNested attrTerms, List<String> nameList) {
        if (attrTerms != null) {
            Aggregations attrAggregations = attrTerms.getAggregations();
            Map<String, Aggregation> attrMap = attrAggregations.getAsMap();
            ParsedStringTerms nameAgg = (ParsedStringTerms) attrMap.get("nameAgg");

            if (nameAgg != null) {
                return this.buildGoodsParamOptions(nameAgg, nameList);
            }

        }
        return new ArrayList<>();
    }

    /**
     * 构造商品参数属性
     *
     * @param nameAgg  商品参数聚合内容
     * @param nameList 查询的规格名
     * @return 商品参数属性集合
     */
    private List<ParamOptions> buildGoodsParamOptions(ParsedStringTerms nameAgg, List<String> nameList) {
        List<ParamOptions> paramOptions = new ArrayList<>();
        List<? extends Terms.Bucket> nameBuckets = nameAgg.getBuckets();

        for (Terms.Bucket bucket : nameBuckets) {
            String name = bucket.getKey().toString();
            ParamOptions paramOptions1 = new ParamOptions();
            ParsedStringTerms valueAgg = bucket.getAggregations().get("valueAgg");
            List<? extends Terms.Bucket> valueBuckets = valueAgg.getBuckets();
            List<String> valueSelectorList = new ArrayList<>();

            for (Terms.Bucket valueBucket : valueBuckets) {
                String value = valueBucket.getKey().toString();

                if (CharSequenceUtil.isNotEmpty(value)) {
                    valueSelectorList.add(value);
                }

            }
            if (nameList == null || !nameList.contains(name)) {
                paramOptions1.setKey(name);
                paramOptions1.setValues(valueSelectorList);
                paramOptions.add(paramOptions1);
            }
        }
        return paramOptions;
    }

    /**
     * 创建es搜索builder
     *
     * @param searchDTO 搜索条件
     * @param pageVo    分页参数
     * @return es搜索builder
     */
    @Override
    public NativeSearchQueryBuilder createSearchQueryBuilder(EsGoodsSearchDTO searchDTO, PageVO pageVo) {
        NativeSearchQueryBuilder nativeSearchQueryBuilder = new NativeSearchQueryBuilder();
        if (pageVo != null) {
            int pageNumber = pageVo.getPageNumber() - 1;
            if (pageNumber < 0) {
                pageNumber = 0;
            }
            Pageable pageable = PageRequest.of(pageNumber, pageVo.getPageSize());
            //分页
            nativeSearchQueryBuilder.withPageable(pageable);
        }
        //查询参数非空判定
        if (searchDTO != null) {
            //过滤条件
            BoolQueryBuilder filterBuilder = QueryBuilders.boolQuery();

            //对查询条件进行处理
            this.commonSearch(filterBuilder, searchDTO);

            //智能推荐
            this.recommended(filterBuilder, searchDTO);

            //未上架的商品不显示
            filterBuilder.must(QueryBuilders.matchQuery("marketEnable", GoodsStatusEnum.UPPER.name()));
            //待审核和审核不通过的商品不显示
            filterBuilder.must(QueryBuilders.matchQuery("authFlag", GoodsAuthEnum.PASS.name()));


            //关键字检索
            if (CharSequenceUtil.isEmpty(searchDTO.getKeyword())) {
                List<FunctionScoreQueryBuilder.FilterFunctionBuilder> filterFunctionBuilders = this.buildFunctionSearch();
                FunctionScoreQueryBuilder.FilterFunctionBuilder[] builders = new FunctionScoreQueryBuilder.FilterFunctionBuilder[filterFunctionBuilders.size()];
                filterFunctionBuilders.toArray(builders);
                FunctionScoreQueryBuilder functionScoreQueryBuilder = QueryBuilders.functionScoreQuery(QueryBuilders.matchAllQuery(), builders)
                        .scoreMode(FunctionScoreQuery.ScoreMode.SUM)
                        .setMinScore(2);
                //聚合搜索则将结果放入过滤条件
                filterBuilder.must(functionScoreQueryBuilder);
            } else {
                this.keywordSearch(filterBuilder, searchDTO.getKeyword());
            }

            //如果是聚合查询

            nativeSearchQueryBuilder.withQuery(filterBuilder);


            if (pageVo != null && CharSequenceUtil.isNotEmpty(pageVo.getOrder()) && CharSequenceUtil.isNotEmpty(pageVo.getSort())) {
                nativeSearchQueryBuilder.withSort(SortBuilders.fieldSort(pageVo.getSort()).order(SortOrder.valueOf(pageVo.getOrder().toUpperCase())));
            } else {
                nativeSearchQueryBuilder.withSort(SortBuilders.scoreSort().order(SortOrder.DESC));
            }

        }
        return nativeSearchQueryBuilder;
    }

    /**
     * 商品推荐
     *
     * @param filterBuilder 过滤条件
     * @param searchDTO     搜索条件
     */
    private void recommended(BoolQueryBuilder filterBuilder, EsGoodsSearchDTO searchDTO) {

        String currentGoodsId = searchDTO.getCurrentGoodsId();
        if (CharSequenceUtil.isEmpty(currentGoodsId)) {
            return;
        }

        //排除当前商品
        filterBuilder.mustNot(QueryBuilders.matchQuery("id", currentGoodsId));

        //查询当前浏览商品的索引信息
        EsGoodsIndex esGoodsIndex = restTemplate.get(currentGoodsId, EsGoodsIndex.class);
        if (esGoodsIndex == null) {
            return;
        }
        //推荐与当前浏览商品相同一个二级分类下的商品
        String categoryPath = esGoodsIndex.getCategoryPath();
        if (CharSequenceUtil.isNotEmpty(categoryPath)) {
            //匹配二级分类
            String substring = categoryPath.substring(0, categoryPath.lastIndexOf(","));
            filterBuilder.must(QueryBuilders.wildcardQuery("categoryPath", substring + "*"));
        }

    }

    /**
     * 查询属性处理
     *
     * @param filterBuilder 过滤构造器
     * @param searchDTO     查询参数
     */
    private void commonSearch(BoolQueryBuilder filterBuilder, EsGoodsSearchDTO searchDTO) {
        //品牌判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getBrandId())) {
            String[] brands = searchDTO.getBrandId().split("@");
            filterBuilder.filter(QueryBuilders.termsQuery(ATTR_BRAND_ID, brands));
        }
        if (searchDTO.getRecommend() != null) {
            filterBuilder.filter(QueryBuilders.termQuery("recommend", searchDTO.getRecommend()));
        }
        // 商品类型判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getGoodsType())) {
            filterBuilder.filter(QueryBuilders.termQuery("goodsType", searchDTO.getGoodsType()));
        }
        if (CharSequenceUtil.isNotEmpty(searchDTO.getNeGoodsType())) {
            filterBuilder.mustNot(QueryBuilders.termQuery("goodsType", searchDTO.getNeGoodsType()));
        }
        // 销售类型判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getSalesModel())) {
            filterBuilder.filter(QueryBuilders.termQuery("salesModel", searchDTO.getSalesModel()));
        }
        if (CharSequenceUtil.isNotEmpty(searchDTO.getNeSalesModel())) {
            filterBuilder.mustNot(QueryBuilders.termQuery("salesModel", searchDTO.getNeSalesModel()));
        }
        //规格项判定
        if (searchDTO.getNameIds() != null && !searchDTO.getNameIds().isEmpty()) {
            filterBuilder.must(QueryBuilders.nestedQuery(ATTR_PATH, QueryBuilders.termsQuery("attrList.nameId", searchDTO.getNameIds()), ScoreMode.None));
        }
        //分类判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getCategoryId())) {
            filterBuilder.must(QueryBuilders.wildcardQuery("categoryPath", "*" + searchDTO.getCategoryId() + "*"));
        }
        //店铺分类判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getStoreCatId())) {
            filterBuilder.must(QueryBuilders.wildcardQuery("storeCategoryPath", "*" + searchDTO.getStoreCatId() + "*"));
        }
        //店铺判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getStoreId())) {
            filterBuilder.filter(QueryBuilders.termQuery("storeId", searchDTO.getStoreId()));
        }
        //属性判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getProp())) {
            this.propSearch(filterBuilder, searchDTO);
        }
        // 促销活动判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getPromotionsId()) && CharSequenceUtil.isNotEmpty(searchDTO.getPromotionType())) {
            filterBuilder.must(QueryBuilders.wildcardQuery("promotionMapJson", "*" + searchDTO.getPromotionType() + "-" + searchDTO.getPromotionsId() + "*"));
        }
        //价格区间判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getPrice())) {
            String[] prices = searchDTO.getPrice().split("_");
            if (prices.length == 0) {
                return;
            }
            double min = Convert.toDouble(prices[0], 0.0);
            double max = Integer.MAX_VALUE;

            if (prices.length == 2) {
                max = Convert.toDouble(prices[1], Double.MAX_VALUE);
            }
            if (min > max) {
                throw new ServiceException("价格区间错误");
            }
            if (min > Double.MAX_VALUE) {
                min = Double.MAX_VALUE;
            }
            if (max > Double.MAX_VALUE) {
                max = Double.MAX_VALUE;
            }
            filterBuilder.must(QueryBuilders.rangeQuery("price").from(min).to(max).includeLower(true).includeUpper(true));
        }
    }

    /**
     * 商品参数查询处理
     *
     * @param filterBuilder 过滤构造器
     * @param searchDTO     查询参数
     */
    private void propSearch(BoolQueryBuilder filterBuilder, EsGoodsSearchDTO searchDTO) {
        String[] props = searchDTO.getProp().split("@");
        List<String> nameList = new ArrayList<>();
        List<String> valueList = new ArrayList<>();
        Map<String, List<String>> valueMap = new HashMap<>(16);
        for (String prop : props) {
            String[] propValues = prop.split("_");
            String name = propValues[0];
            String value = propValues[1];
            if (!nameList.contains(name)) {
                nameList.add(name);
            }
            if (!valueList.contains(value)) {
                valueList.add(value);
            }
            //将同一规格名下的规格值分组
            if (!valueMap.containsKey(name)) {
                List<String> values = new ArrayList<>();
                values.add(value);
                valueMap.put(name, values);
            } else {
                valueMap.get(name).add(value);
            }
        }
        //遍历所有的规格
        for (Map.Entry<String, List<String>> entry : valueMap.entrySet()) {
            filterBuilder.must(QueryBuilders.nestedQuery(ATTR_PATH, QueryBuilders.matchQuery(ATTR_NAME, entry.getKey()), ScoreMode.None));
            BoolQueryBuilder shouldBuilder = QueryBuilders.boolQuery();
            for (String s : entry.getValue()) {
                shouldBuilder.should(QueryBuilders.nestedQuery(ATTR_PATH, QueryBuilders.matchQuery(ATTR_VALUE, s), ScoreMode.None));
            }
            filterBuilder.must(shouldBuilder);
        }
        searchDTO.getNotShowCol().put(ATTR_NAME_KEY, nameList);
        searchDTO.getNotShowCol().put(ATTR_VALUE_KEY, valueList);
    }

    /**
     * 关键字查询处理
     *
     * @param filterBuilder 过滤构造器
     * @param keyword       关键字
     */
    private void keywordSearch(BoolQueryBuilder filterBuilder, String keyword) {

        List<FunctionScoreQueryBuilder.FilterFunctionBuilder> filterFunctionBuilders = this.buildFunctionSearch();

        //分词匹配
        // operator 为 AND 时 需全部分词匹配。为 OR 时 需配置 minimumShouldMatch（最小分词匹配数）不设置默认为1
        MatchQueryBuilder goodsNameMatchQuery = QueryBuilders.matchQuery("goodsName", keyword).operator(Operator.OR).minimumShouldMatch(MINIMUM_SHOULD_MATCH);

        FunctionScoreQueryBuilder.FilterFunctionBuilder[] builders = new FunctionScoreQueryBuilder.FilterFunctionBuilder[filterFunctionBuilders.size()];
        filterFunctionBuilders.toArray(builders);
        FunctionScoreQueryBuilder functionScoreQueryBuilder = QueryBuilders.functionScoreQuery(goodsNameMatchQuery, builders)
                .scoreMode(FunctionScoreQuery.ScoreMode.SUM)
                .setMinScore(2);
        //聚合搜索则将结果放入过滤条件
        filterBuilder.must(functionScoreQueryBuilder);
        filterBuilder.should(QueryBuilders.boolQuery().should(QueryBuilders.matchPhraseQuery("goodsName", keyword).boost(10)));
    }

    /**
     * 构造关键字查询
     *
     * @return 构造查询的集合
     */
    private List<FunctionScoreQueryBuilder.FilterFunctionBuilder> buildFunctionSearch() {
        List<FunctionScoreQueryBuilder.FilterFunctionBuilder> filterFunctionBuilders = new ArrayList<>();

        // 修改分数算法为无，数字最大分数越高
        FieldValueFactorFunctionBuilder skuNoScore = ScoreFunctionBuilders.fieldValueFactorFunction("skuSource").modifier(FieldValueFactorFunction.Modifier.LOG1P).setWeight(3);
        FunctionScoreQueryBuilder.FilterFunctionBuilder skuNoBuilder = new FunctionScoreQueryBuilder.FilterFunctionBuilder(skuNoScore);
        filterFunctionBuilders.add(skuNoBuilder);

        return filterFunctionBuilders;
    }

}
