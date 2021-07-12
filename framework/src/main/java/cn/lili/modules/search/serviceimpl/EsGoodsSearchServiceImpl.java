package cn.lili.modules.search.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.cache.Cache;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.Brand;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.BrandService;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.entity.dos.EsGoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import cn.lili.modules.search.entity.dto.ParamOptions;
import cn.lili.modules.search.entity.dto.SelectorOptions;
import cn.lili.modules.search.entity.enums.HotWordsRedisKeyEnum;
import cn.lili.modules.search.repository.EsGoodsIndexRepository;
import cn.lili.modules.search.service.EsGoodsSearchService;
import lombok.extern.slf4j.Slf4j;
import org.apache.lucene.search.join.ScoreMode;
import org.elasticsearch.common.lucene.search.function.FunctionScoreQuery;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.index.query.functionscore.FunctionScoreQueryBuilder;
import org.elasticsearch.index.query.functionscore.ScoreFunctionBuilders;
import org.elasticsearch.search.aggregations.Aggregation;
import org.elasticsearch.search.aggregations.AggregationBuilder;
import org.elasticsearch.search.aggregations.AggregationBuilders;
import org.elasticsearch.search.aggregations.Aggregations;
import org.elasticsearch.search.aggregations.bucket.nested.ParsedNested;
import org.elasticsearch.search.aggregations.bucket.terms.ParsedStringTerms;
import org.elasticsearch.search.aggregations.bucket.terms.Terms;
import org.elasticsearch.search.sort.SortBuilders;
import org.elasticsearch.search.sort.SortOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHits;
import org.springframework.data.elasticsearch.core.query.NativeSearchQuery;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.redis.core.DefaultTypedTuple;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * ES商品搜索业务层实现
 * @author paulG
 * @since 2020/10/16
 **/
@Slf4j
@Service
public class EsGoodsSearchServiceImpl implements EsGoodsSearchService {

    private static final String ATTR_PATH = "attrList";
    private static final String ATTR_VALUE = "attrList.value";
    private static final String ATTR_NAME = "attrList.name";
    private static final String ATTR_NAME_KEY = "nameList";
    private static final String ATTR_VALUE_KEY = "valueList";

    @Autowired
    private EsGoodsIndexRepository goodsIndexRepository;
    /**
     * 商品分类
     */
    @Autowired
    private CategoryService categoryService;
    /**
     * 品牌
     */
    @Autowired
    private BrandService brandService;
    /**
     * ES
     */
    @Autowired
    private ElasticsearchRestTemplate restTemplate;
    /**
     * 缓存
     */
    @Autowired
    private Cache cache;

    @Override
    public Page<EsGoodsIndex> searchGoods(EsGoodsSearchDTO searchDTO, PageVO pageVo) {
        if (CharSequenceUtil.isNotEmpty(searchDTO.getKeyword())) {
            cache.incrementScore(HotWordsRedisKeyEnum.SEARCH_HOT_WORD.name(), searchDTO.getKeyword());
        }
        NativeSearchQueryBuilder searchQueryBuilder = createSearchQueryBuilder(searchDTO, pageVo, true);
        NativeSearchQuery searchQuery = searchQueryBuilder.build();
        log.info("searchGoods DSL:{}", searchQuery.getQuery());

        return goodsIndexRepository.search(searchQuery);
    }

    @Override
    public List<String> getHotWords(Integer start, Integer end) {
        List<String> hotWords = new ArrayList<>();
        Set<DefaultTypedTuple> set = cache.reverseRangeWithScores(HotWordsRedisKeyEnum.SEARCH_HOT_WORD.name(), start, end);
        for (DefaultTypedTuple defaultTypedTuple : set) {
            hotWords.add(Objects.requireNonNull(defaultTypedTuple.getValue()).toString());
        }
        return hotWords;
    }

    @Override
    public EsGoodsRelatedInfo getSelector(EsGoodsSearchDTO goodsSearch, PageVO pageVo) {
        NativeSearchQueryBuilder builder = createSearchQueryBuilder(goodsSearch, null, true);
        //分类
        builder.addAggregation(AggregationBuilders.terms("categoryAgg").field("categoryPath"));
        //品牌
        builder.addAggregation(AggregationBuilders.terms("brandAgg").field("brandId").size(Integer.MAX_VALUE));
        //参数
        AggregationBuilder valuesBuilder = AggregationBuilders.terms("valueAgg").field(ATTR_VALUE);
        AggregationBuilder paramsNameBuilder = AggregationBuilders.terms("nameAgg").field(ATTR_NAME).subAggregation(valuesBuilder);
        builder.addAggregation(AggregationBuilders.nested("attrAgg", ATTR_PATH).subAggregation(paramsNameBuilder));
        NativeSearchQuery searchQuery = builder.build();
        SearchHits<EsGoodsIndex> search = restTemplate.search(searchQuery, EsGoodsIndex.class);

        log.info("getSelector DSL:{}", searchQuery.getQuery());
        Map<String, Aggregation> aggregationMap = Objects.requireNonNull(search.getAggregations()).getAsMap();
        return convertToEsGoodsRelatedInfo(aggregationMap, goodsSearch);
    }

    @Override
    public List<EsGoodsIndex> getEsGoodsBySkuIds(List<String> skuIds) {
        NativeSearchQueryBuilder searchQueryBuilder = new NativeSearchQueryBuilder();
        NativeSearchQuery build = searchQueryBuilder.build();
        build.setIds(skuIds);
        return restTemplate.multiGet(build, EsGoodsIndex.class, restTemplate.getIndexCoordinatesFor(EsGoodsIndex.class));
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
            for (Terms.Bucket categoryBucket : categoryBuckets) {
                String categoryPath = categoryBucket.getKey().toString();
                String[] split = categoryPath.split(",");
                for (String s : split) {
                    if (CharSequenceUtil.isNotEmpty(s)) {
                        Category category = categoryService.getById(s);
                        if (category != null) {
                            SelectorOptions so = new SelectorOptions();
                            so.setName(category.getName());
                            so.setValue(category.getId());
                            if (!categoryOptions.contains(so)) {
                                categoryOptions.add(so);
                            }
                        }
                    }
                }

            }
        }
        esGoodsRelatedInfo.setCategories(categoryOptions);

        //品牌
        ParsedStringTerms brandTerms = (ParsedStringTerms) aggregationMap.get("brandAgg");
        List<? extends Terms.Bucket> brandBuckets = brandTerms.getBuckets();
        List<SelectorOptions> brandOptions = new ArrayList<>();
        if (brandBuckets != null && !brandBuckets.isEmpty()) {
            for (Terms.Bucket brandBucket : brandBuckets) {
                if (CharSequenceUtil.isNotEmpty(goodsSearch.getBrandId())) {
                    List<String> brandList = Arrays.asList(goodsSearch.getBrandId().split("@"));
                    if (brandList.contains(brandBucket.getKey().toString())) {
                        continue;
                    }
                }
                Brand brand = brandService.getById(brandBucket.getKey().toString());
                if (brand != null) {
                    SelectorOptions so = new SelectorOptions();
                    so.setName(brand.getName());
                    so.setValue(brand.getId());
                    so.setUrl(brand.getLogo());
                    brandOptions.add(so);
                }
            }
        }
        esGoodsRelatedInfo.setBrands(brandOptions);

        //参数
        ParsedNested attrTerms = (ParsedNested) aggregationMap.get("attrAgg");
        if (!goodsSearch.getNotShowCol().isEmpty()) {
            if (goodsSearch.getNotShowCol().containsKey(ATTR_NAME_KEY) && goodsSearch.getNotShowCol().containsKey(ATTR_VALUE_KEY)) {
                esGoodsRelatedInfo.setParamOptions(buildGoodsParam(attrTerms, goodsSearch.getNotShowCol().get(ATTR_NAME_KEY), goodsSearch.getNotShowCol().get(ATTR_VALUE_KEY)));
            }
        } else {
            esGoodsRelatedInfo.setParamOptions(buildGoodsParam(attrTerms, null, null));
        }

        return esGoodsRelatedInfo;
    }

    /**
     * 构建商品参数信息
     *
     * @param attrTerms 商品参数搜索结果
     * @param nameList  查询的规格名
     * @param valueList 查询的规格项
     * @return 商品参数信息
     */
    private List<ParamOptions> buildGoodsParam(ParsedNested attrTerms, List<String> nameList, List<String> valueList) {
        List<ParamOptions> paramOptions = new ArrayList<>();
        if (attrTerms != null) {
            Aggregations attrAggregations = attrTerms.getAggregations();
            Map<String, Aggregation> attrMap = attrAggregations.getAsMap();
            ParsedStringTerms nameAgg = (ParsedStringTerms) attrMap.get("nameAgg");

            if (nameAgg != null) {
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


            }

        }
        return paramOptions;
    }


    /**
     * 创建es搜索builder
     *
     * @param searchDTO     搜索条件
     * @param pageVo        分页参数
     * @param isAggregation 是否是聚合查询
     * @return es搜索builder
     */
    private NativeSearchQueryBuilder createSearchQueryBuilder(EsGoodsSearchDTO searchDTO, PageVO pageVo, boolean isAggregation) {
        NativeSearchQueryBuilder nativeSearchQueryBuilder = new NativeSearchQueryBuilder();
        if (pageVo != null) {
            Integer pageNumber = pageVo.getPageNumber() - 1;
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
            //查询条件
            BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();

            //对查询条件进行处理
            this.commonSearch(filterBuilder, queryBuilder, searchDTO, isAggregation);

            //未上架的商品不显示
            filterBuilder.must(QueryBuilders.matchQuery("marketEnable", GoodsStatusEnum.UPPER.name()));
            //待审核和审核不通过的商品不显示
            filterBuilder.must(QueryBuilders.matchQuery("isAuth", GoodsAuthEnum.PASS.name()));


            //关键字检索
            if (StringUtils.isEmpty(searchDTO.getKeyword())) {
                nativeSearchQueryBuilder.withQuery(QueryBuilders.matchAllQuery());
            } else {
                this.keywordSearch(filterBuilder, queryBuilder, searchDTO.getKeyword(), isAggregation);
            }

            //如果是聚合查询
            if (isAggregation) {
                nativeSearchQueryBuilder.withQuery(filterBuilder);
            } else {
                nativeSearchQueryBuilder.withQuery(queryBuilder);
                nativeSearchQueryBuilder.withFilter(filterBuilder);
            }


            if (pageVo != null && CharSequenceUtil.isNotEmpty(pageVo.getOrder()) && CharSequenceUtil.isNotEmpty(pageVo.getSort())) {
                nativeSearchQueryBuilder.withSort(SortBuilders.fieldSort(pageVo.getSort()).order(SortOrder.valueOf(pageVo.getOrder().toUpperCase())));
            } else {
                nativeSearchQueryBuilder.withSort(SortBuilders.scoreSort().order(SortOrder.DESC));
            }

        }
        return nativeSearchQueryBuilder;
    }

    /**
     * 查询属性处理
     * @param filterBuilder
     * @param queryBuilder
     * @param searchDTO
     * @param isAggregation
     */
    private void commonSearch(BoolQueryBuilder filterBuilder, BoolQueryBuilder queryBuilder, EsGoodsSearchDTO searchDTO, boolean isAggregation) {
        //品牌判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getBrandId())) {
            String[] brands = searchDTO.getBrandId().split("@");
            filterBuilder.must(QueryBuilders.termsQuery("brandId", brands));
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
            BoolQueryBuilder usedQueryBuilder;
            if (isAggregation) {
                usedQueryBuilder = filterBuilder;
            } else {
                usedQueryBuilder = queryBuilder;
            }
            //遍历所有的规格
            for (Map.Entry<String, List<String>> entry : valueMap.entrySet()) {
                usedQueryBuilder.must(QueryBuilders.nestedQuery(ATTR_PATH, QueryBuilders.wildcardQuery(ATTR_NAME, "*" + entry.getKey() + "*"), ScoreMode.None));
                BoolQueryBuilder shouldBuilder = QueryBuilders.boolQuery();
                for (String s : entry.getValue()) {
                    shouldBuilder.should(QueryBuilders.nestedQuery(ATTR_PATH, QueryBuilders.wildcardQuery(ATTR_VALUE, "*" + s + "*"), ScoreMode.None));
                }
                usedQueryBuilder.must(shouldBuilder);
            }
            searchDTO.getNotShowCol().put(ATTR_NAME_KEY, nameList);
            searchDTO.getNotShowCol().put(ATTR_VALUE_KEY, valueList);
        }
        //价格区间判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getPrice())) {
            String[] prices = searchDTO.getPrice().split("_");
            if(prices.length==0){
                return;
            }
            double min = StringUtils.toDouble(prices[0], 0.0);
            double max = Integer.MAX_VALUE;

            if (prices.length == 2) {
                max = StringUtils.toDouble(prices[1], Double.MAX_VALUE);
            }
            filterBuilder.must(QueryBuilders.rangeQuery("price").from(min).to(max).includeLower(true).includeUpper(true));
        }
    }

    /**
     * 关键字查询处理
     * @param filterBuilder
     * @param queryBuilder
     * @param keyword
     * @param isAggregation
     */
    private void keywordSearch(BoolQueryBuilder filterBuilder, BoolQueryBuilder queryBuilder, String keyword, boolean isAggregation) {
        List<FunctionScoreQueryBuilder.FilterFunctionBuilder> filterFunctionBuilders = new ArrayList<>();
        //商品名字匹配
        filterFunctionBuilders.add(new FunctionScoreQueryBuilder.FilterFunctionBuilder(QueryBuilders.wildcardQuery("goodsName", "*" + keyword + "*"),
                ScoreFunctionBuilders.weightFactorFunction(10)));
        //属性匹配
        filterFunctionBuilders.add(new FunctionScoreQueryBuilder.FilterFunctionBuilder(QueryBuilders.nestedQuery(ATTR_PATH, QueryBuilders.wildcardQuery(ATTR_VALUE, "*" + keyword + "*"), ScoreMode.None),
                ScoreFunctionBuilders.weightFactorFunction(8)));

        FunctionScoreQueryBuilder.FilterFunctionBuilder[] builders = new FunctionScoreQueryBuilder.FilterFunctionBuilder[filterFunctionBuilders.size()];
        filterFunctionBuilders.toArray(builders);
        FunctionScoreQueryBuilder functionScoreQueryBuilder = QueryBuilders.functionScoreQuery(builders)
                .scoreMode(FunctionScoreQuery.ScoreMode.SUM)
                .setMinScore(2);
        //聚合搜索则将结果放入过滤条件
        if (isAggregation) {
            filterBuilder.must(functionScoreQueryBuilder);
        }
        //否则放入查询条件
        else {
            queryBuilder.must(functionScoreQueryBuilder);
        }
    }

}
