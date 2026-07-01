package cn.lili.modules.search.service;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ArrayUtil;
import cn.hutool.extra.pinyin.PinyinUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.elasticsearch.BaseElasticsearchService;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.search.entity.dos.EsGoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import cn.lili.modules.search.entity.dto.ParamOptions;
import cn.lili.modules.search.entity.dto.SelectorOptions;
import co.elastic.clients.elasticsearch._types.FieldValue;
import co.elastic.clients.elasticsearch._types.aggregations.*;
import co.elastic.clients.elasticsearch._types.query_dsl.FieldValueFactorModifier;
import co.elastic.clients.elasticsearch._types.query_dsl.Query;
import co.elastic.clients.elasticsearch._types.query_dsl.QueryBuilders;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.elasticsearch.client.elc.ElasticsearchAggregation;
import org.springframework.data.elasticsearch.client.elc.NativeQuery;
import org.springframework.data.elasticsearch.client.elc.NativeQueryBuilder;

import java.util.*;

/**
 * @author paulG
 * @since 2022/8/3
 **/
@Slf4j
public class EsGoodsSearchAbstractService extends BaseElasticsearchService {

    // 最小分词匹配
    public static final String MINIMUM_SHOULD_MATCH = "100%";

    public static final String SPU_NAME = "spuName";
    public static final String ATTR_PATH = "attrList";
    public static final String ATTR_VALUE = "attrList.value";
    public static final String ATTR_NAME = "attrList.name";
    public static final String ATTR_SORT = "attrList.sort";
    public static final String ATTR_AGG = "attrAgg";
    public static final String ATTR_BRAND_ID_NAME_AGG = "brandIdNameAgg";
    public static final String ATTR_BRAND_ID_URL_AGG = "brandUrlNameAgg";
    public static final String ATTR_BRAND_ID_FIELD = "brandId";
    public static final String ATTR_BRAND_NAME_AGG = "brandNameAgg";
    // 修正：聚合字段不使用 .keyword 子字段
    public static final String ATTR_BRAND_NAME_FIELD = "brandName.keyword";
    public static final String ATTR_BRAND_URL_AGG = "brandUrlAgg";
    // 修正：聚合字段不使用 .keyword 子字段
    public static final String ATTR_BRAND_URL_FIELD = "brandUrl.keyword";
    public static final String ATTR_NAME_KEY = "nameList";
    public static final String ATTR_VALUE_KEY = "valueList";
    public static final String ATTR_CATEGORY_NAME_AGG = "categoryNameAgg";
    // 修正：聚合字段不使用 .keyword 子字段
    public static final String ATTR_CATEGORY_NAME_FIELD = "categoryNamePath.keyword";
    public static final String ATTR_CATEGORY_PATH_AGG = "categoryPathAgg";
    public static final String ATTR_CATEGORY_PATH_FIELD = "categoryPath";
    public static final String GOODS_NAME = "goodsName";
    public static final String SKU_SOURCE = "skuSource";

    /**
     * 转换搜索结果为聚合商品展示信息
     *
     * @param aggregationMap 搜索结果
     * @return 聚合商品展示信息
     */
    public EsGoodsRelatedInfo convertToEsGoodsRelatedInfo(Map<String, ElasticsearchAggregation> aggregationMap,
                                                          EsGoodsSearchDTO goodsSearch) {
        EsGoodsRelatedInfo esGoodsRelatedInfo = new EsGoodsRelatedInfo();
        //分类
        if (aggregationMap.get(ATTR_CATEGORY_PATH_AGG) != null) {
            esGoodsRelatedInfo.setCategories(this.convertCategoryOptions(aggregationMap));
        }

        //品牌
        if (aggregationMap.get(ATTR_BRAND_ID_NAME_AGG) != null && aggregationMap.get(ATTR_BRAND_ID_URL_AGG) != null) {
            esGoodsRelatedInfo.setBrands(this.convertBrandOptions(goodsSearch, aggregationMap));
        }

        //参数
        if (!goodsSearch.getNotShowCol().isEmpty()) {
            if (goodsSearch.getNotShowCol().containsKey(ATTR_NAME_KEY) && goodsSearch.getNotShowCol()
                    .containsKey(ATTR_VALUE_KEY)) {
                esGoodsRelatedInfo.setParamOptions(
                        buildGoodsParam(aggregationMap, goodsSearch.getNotShowCol().get(ATTR_NAME_KEY)));
            }
        } else {
            esGoodsRelatedInfo.setParamOptions(buildGoodsParam(aggregationMap, null));
        }

        return esGoodsRelatedInfo;
    }

    /**
     * 将品牌聚合结果转换品牌选择项
     *
     * @param goodsSearch    查询参数
     * @param aggregationMap 聚合结果
     * @return 品牌选择项列表
     */
    private List<SelectorOptions> convertBrandOptions(EsGoodsSearchDTO goodsSearch,
                                                      Map<String, ElasticsearchAggregation> aggregationMap) {
        try {
            List<SelectorOptions> brandOptions = new ArrayList<>();

            Aggregate aggregateBrandIdName = aggregationMap.get(ATTR_BRAND_ID_NAME_AGG).aggregation().getAggregate();
            Aggregate aggregateBrandUrlName = aggregationMap.get(ATTR_BRAND_ID_URL_AGG).aggregation().getAggregate();

            StringTermsAggregate brandIdNameAgg = (StringTermsAggregate) aggregateBrandIdName._get();
            StringTermsAggregate brandUrlAgg = (StringTermsAggregate) aggregateBrandUrlName._get();

            Buckets<StringTermsBucket> brandIdBuckets = brandIdNameAgg.buckets();
            Buckets<StringTermsBucket> brandUrlBuckets = brandUrlAgg.buckets();

            for (int i = 0; i < brandIdBuckets.array().size(); i++) {
                StringTermsBucket brandIdBucket = brandIdBuckets.array().get(i);
                StringTermsBucket brandUrlBucket = brandUrlBuckets.array().get(i);
                FieldValue key = brandIdBucket.key();
                String brandId = key.stringValue();

                //当品牌id为空并且
                if (brandId.equals("0") || (CharSequenceUtil.isNotEmpty(goodsSearch.getBrandId()) && Arrays.asList(
                        goodsSearch.getBrandId().split("@")).contains(brandId))) {
                    continue;
                }
                StringTermsAggregate brandNameTermsAgg =
                        (StringTermsAggregate) brandIdBucket.aggregations().get(ATTR_BRAND_NAME_AGG)._get();
                StringTermsAggregate brandUrlTermsAgg =
                        (StringTermsAggregate) brandUrlBucket.aggregations().get(ATTR_BRAND_URL_AGG)._get();
                if (brandNameTermsAgg == null || brandUrlTermsAgg == null || brandNameTermsAgg.buckets().array()
                        .isEmpty() || brandUrlTermsAgg.buckets().array().isEmpty()
                        || brandNameTermsAgg.buckets().array().get(0).key() == null
                        || brandUrlTermsAgg.buckets().array().get(0).key() == null) {
                    continue;
                }
                String brandName = brandNameTermsAgg.buckets().array().get(0).key().stringValue();
                String brandUrl = brandUrlTermsAgg.buckets().array().get(0).key().stringValue();
                SelectorOptions so = new SelectorOptions();
                so.setName(brandName);
                so.setValue(brandId);
                so.setUrl(brandUrl);
                brandOptions.add(so);
            }
            return brandOptions;
        } catch (Exception e) {
            log.error("品牌聚合结果转换异常", e);
            return new ArrayList<>();
        }
    }

    /**
     * 将分类聚合结果转换分类选择项
     *
     * @param aggregationMap 分类聚合结果
     * @return 分类选择项集合
     */
    private List<SelectorOptions> convertCategoryOptions(Map<String, ElasticsearchAggregation> aggregationMap) {
        try {
            List<SelectorOptions> categoryOptions = new ArrayList<>();

            Aggregate categoryIdAgg = aggregationMap.get(ATTR_CATEGORY_PATH_AGG).aggregation().getAggregate();

            StringTermsAggregate categoryAgg = (StringTermsAggregate) categoryIdAgg._get();
            if (categoryAgg == null) {
                return categoryOptions;
            }
            Buckets<StringTermsBucket> buckets = categoryAgg.buckets();
            for (StringTermsBucket stringTermsBucket : buckets.array()) {
                FieldValue key = stringTermsBucket.key();

                String categoryPath = key.stringValue();

                Aggregate categoryNameAgg = stringTermsBucket.aggregations().get(ATTR_CATEGORY_NAME_AGG);
                StringTermsAggregate categoryNameTermAgg = (StringTermsAggregate) categoryNameAgg._get();
                if (categoryNameTermAgg == null) {
                    continue;
                }
                Buckets<StringTermsBucket> categoryNameBuckets = categoryNameTermAgg.buckets();
                if (categoryNameBuckets == null || !categoryNameBuckets.isArray() || categoryNameBuckets.array()
                        .isEmpty()) {
                    continue;
                }
                for (StringTermsBucket nameBucket : categoryNameBuckets.array()) {
                    FieldValue categoryNameField = nameBucket.key();

                    String categoryNamePath = categoryNameField.stringValue();
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
        } catch (Exception e) {
            log.error("分类聚合结果转换异常", e);
            return new ArrayList<>();
        }
    }

    /**
     * 构建商品参数信息
     *
     * @param aggregationMap 聚合结果
     * @param nameList       查询的规格名
     * @return 商品参数信息
     */
    private List<ParamOptions> buildGoodsParam(Map<String, ElasticsearchAggregation> aggregationMap,
                                               List<String> nameList) {
        try {
            if (aggregationMap.get(ATTR_AGG) != null) {
                Aggregate attrTerms = aggregationMap.get(ATTR_AGG).aggregation().getAggregate();
                NestedAggregate nestedAggregate = (NestedAggregate) attrTerms._get();

                StringTermsAggregate nameAgg =
                        (StringTermsAggregate) nestedAggregate.aggregations().get("nameAgg")._get();
                if (nameAgg != null && nameAgg.buckets().isArray() && !nameAgg.buckets().array().isEmpty()) {
                    return this.buildGoodsParamOptions(nameAgg, nameList);
                }

            }
        } catch (Exception e) {
            log.error("商品参数聚合结果转换异常", e);
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
    private List<ParamOptions> buildGoodsParamOptions(StringTermsAggregate nameAgg, List<String> nameList) {
        List<ParamOptions> paramOptions = new ArrayList<>();

        for (StringTermsBucket bucket : nameAgg.buckets().array()) {
            String name = bucket.key().stringValue();
            ParamOptions paramOptions1 = new ParamOptions();
            Aggregate aggregate = bucket.aggregations().get("valueAgg");
            StringTermsAggregate stringTermsBucket = (StringTermsAggregate) aggregate._get();

            List<String> valueSelectorList = new ArrayList<>();

            for (StringTermsBucket valueBucket : stringTermsBucket.buckets().array()) {
                String description = valueBucket.key().stringValue();

                if (CharSequenceUtil.isNotEmpty(description)) {
                    valueSelectorList.add(description);
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
    public NativeQueryBuilder createSearchQueryBuilder(EsGoodsSearchDTO searchDTO, PageVO pageVo) {
        NativeQueryBuilder builder = NativeQuery.builder();
        if (pageVo != null) {
            int pageNumber = pageVo.getPageNumber() - 1;
            if (pageNumber < 0) {
                pageNumber = 0;
            }
            Pageable pageable = PageRequest.of(pageNumber, pageVo.getPageSize());
            //分页
            builder.withPageable(pageable);
        }
        //查询参数非空判定
        if (searchDTO != null) {

            //过滤条件

            //对查询条件进行处理
            List<Query> mustList = new ArrayList<>(this.commonSearch(searchDTO));

            //未上架的商品不显示
            mustList.add(QueryBuilders.match(a -> a.field("marketEnable").query(GoodsStatusEnum.UPPER.name())));

            //待审核和审核不通过的商品不显示
            mustList.add(QueryBuilders.match(a -> a.field("authFlag").query(GoodsAuthEnum.PASS.name())));

            //关键字检索
            if (CharSequenceUtil.isEmpty(searchDTO.getKeyword())) {
                if (pageVo != null && !CharSequenceUtil.isNotEmpty(pageVo.getSort())) {
                    mustList.add(this.buildFunctionSearch(searchDTO.getKeyword()));
                }
            } else {
                mustList.add(this.keywordSearch(searchDTO.getKeyword()));
            }

            builder.withQuery(q -> q.bool(b -> b.must(mustList)));

            //如果是聚合查询
            if (pageVo != null && CharSequenceUtil.isNotEmpty(pageVo.getOrder()) && CharSequenceUtil.isNotEmpty(
                    pageVo.getSort())) {
                builder.withSort(Sort.by(
                        Sort.Order.by(pageVo.getSort()).with(Sort.Direction.valueOf(pageVo.getOrder().toUpperCase()))));
            }

        }
        return builder;
    }

    /**
     * 查询属性处理
     *
     * @param searchDTO 查询参数
     */
    public List<Query> commonSearch(EsGoodsSearchDTO searchDTO) {
        List<Query> queries = new ArrayList<>();
        //品牌判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getBrandId())) {
            String[] brands = searchDTO.getBrandId().split("@");
            List<FieldValue> fieldValueList = new ArrayList<>();
            for (String brand : brands) {
                fieldValueList.add(FieldValue.of(brand));
            }
            queries.add(QueryBuilders.terms(a -> a.field(ATTR_BRAND_ID_FIELD).terms(t -> t.value(fieldValueList))));
        }
        if (searchDTO.getRecommend() != null) {
            queries.add(QueryBuilders.term(a -> a.field("recommend").value(searchDTO.getRecommend())));
        }
        //规格项判定
        if (searchDTO.getNameIds() != null && !searchDTO.getNameIds().isEmpty()) {
            List<FieldValue> fieldValueList = new ArrayList<>();
            for (String nameId : searchDTO.getNameIds()) {
                fieldValueList.add(FieldValue.of(nameId));
            }
            queries.add(QueryBuilders.nested(a -> a.path(ATTR_PATH)
                    .query(b -> b.terms(c -> c.field("attrList.nameId").terms(t -> t.value(fieldValueList))))));
        }
        //分类判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getCategoryId())) {
            queries.add(QueryBuilders.wildcard(
                    a -> a.field(ATTR_CATEGORY_PATH_FIELD).value("*" + searchDTO.getCategoryId() + "*")));
        }
        //店铺分类判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getStoreCatId())) {
            queries.add(
                    QueryBuilders.wildcard(a -> a.field("storeCategoryPath").value("*" + searchDTO.getStoreCatId() + "*")));
        }
        if (CharSequenceUtil.isNotEmpty(searchDTO.getIds())) {
            String[] ids = searchDTO.getIds().split(",");
            List<FieldValue> fieldValueList = new ArrayList<>();
            for (String id : ids) {
                fieldValueList.add(FieldValue.of(id));
            }
            queries.add(QueryBuilders.terms(a -> a.field("id").terms(t -> t.value(fieldValueList))));
        }

        //店铺判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getStoreId())) {
            queries.add(QueryBuilders.term(a -> a.field("storeId").value(searchDTO.getStoreId())));
        }

        //属性判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getProp())) {
            queries.addAll(this.propSearch(searchDTO));
        }
        // 促销活动判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getPromotionsId()) && CharSequenceUtil.isNotEmpty(
                searchDTO.getPromotionType())) {
            queries.add(QueryBuilders.wildcard(a -> a.field("promotionMapJson")
                    .value("*" + searchDTO.getPromotionType() + "-" + searchDTO.getPromotionsId() + "*")));
        }

        //价格区间判定
        if (CharSequenceUtil.isNotEmpty(searchDTO.getPrice())) {
            String[] prices = searchDTO.getPrice().split("_");
            if (prices.length > 0) {
                double min = Convert.toDouble(prices[0], 0.0);
                double max;

                if (prices.length == 2) {
                    max = Convert.toDouble(prices[1], Double.MAX_VALUE);
                } else {
                    max = Integer.MAX_VALUE;
                }
                queries.add(
                        co.elastic.clients.elasticsearch._types.query_dsl.RangeQueryBuilders.number(n -> n.field("price").gte(min).lte(max))._toQuery());
            }

        }
        return queries;
    }

    /**
     * 商品参数查询处理
     *
     * @param searchDTO 查询参数
     */
    public List<Query> propSearch(EsGoodsSearchDTO searchDTO) {
        List<Query> queries = new ArrayList<>();

        String[] props = searchDTO.getProp().split("@");
        List<String> nameList = new ArrayList<>();
        List<String> valueList = new ArrayList<>();
        Map<String, List<String>> valueMap = new HashMap<>(16);
        for (String prop : props) {
            String[] propValues = prop.split("_");
            String name = propValues[0];
            String description = propValues[1];
            if (!nameList.contains(name)) {
                nameList.add(name);
            }
            if (!valueList.contains(description)) {
                valueList.add(description);
            }
            //将同一规格名下的规格值分组
            if (!valueMap.containsKey(name)) {
                List<String> values = new ArrayList<>();
                values.add(description);
                valueMap.put(name, values);
            } else {
                valueMap.get(name).add(description);
            }
        }
        //遍历所有的规格
        for (Map.Entry<String, List<String>> entry : valueMap.entrySet()) {
            queries.add(QueryBuilders.nested(
                    a -> a.path(ATTR_PATH).query(b -> b.match(c -> c.field(ATTR_NAME).query(entry.getKey())))));

            for (String s : entry.getValue()) {
                queries.add(QueryBuilders.nested(
                        a -> a.path(ATTR_PATH).query(b -> b.match(c -> c.field(ATTR_VALUE).query(s)))));
            }
        }
        searchDTO.getNotShowCol().put(ATTR_NAME_KEY, nameList);
        searchDTO.getNotShowCol().put(ATTR_VALUE_KEY, valueList);
        return queries;
    }

    /**
     * 关键字查询处理
     *
     * @param keyword 关键字
     */
    public Query keywordSearch(String keyword) {

        String pinyin = PinyinUtil.getFirstLetter(keyword, "");

        List<Query> disMaxQueries = new ArrayList<>();


        // 分词匹配：不再使用未定义的自定义 analyzer，依赖字段自身的 analyzer
        disMaxQueries.add(
                QueryBuilders.match(m -> m.field(GOODS_NAME).query(keyword)
                        .minimumShouldMatch(MINIMUM_SHOULD_MATCH).boost(1.0f)));

        // 拼音相关查询保持注释（待有对应分析器时再开放）
        // disMaxQueries.add(QueryBuilders.term(m -> m.field("goodsName.SPY").value(pinyin)));
        // if (pinyin.length() > 1) {
        //     disMaxQueries.add(QueryBuilders.wildcard(m -> m.field("goodsName.SPY").value("*" + pinyin + "*").boost(0.8f)));
        // }
        // if (CharSequenceUtil.isNotEmpty(keyword)) {
        //     disMaxQueries.add(QueryBuilders.matchPhrase(
        //         m -> m.field("goodsName.FPY").query(PinyinUtil.getPinyin(keyword))
        //             .analyzer("pinyiFullSearchAnalyzer")));
        // }

        // 评分函数（结合基础评分），不再依赖 goodsName.IKS
        disMaxQueries.add(this.buildFunctionSearch(keyword));

        // 聚合搜索则将结果放入过滤条件
        return QueryBuilders.disMax(d -> d.queries(disMaxQueries));
    }

    public Query buildFunctionSearch(String keyword) {
        // missing(1d)：部分文档缺少 skuSource 字段值时，用默认值兜底，
        // 避免 ES field_value_factor 抛出 "Missing value for field [skuSource]" 导致整个查询 500。
        if (CharSequenceUtil.isNotEmpty(keyword)) {
            return QueryBuilders.functionScore(f -> f.query(fu -> fu.match(ff -> ff.field(GOODS_NAME).query(keyword)))
                    .functions(
                            fu -> fu.fieldValueFactor(ff -> ff.field(SKU_SOURCE).modifier(FieldValueFactorModifier.Log1p).missing(1d))
                                    .weight(4d)));
        } else {
            return QueryBuilders.functionScore(f -> f.functions(
                    fu -> fu.fieldValueFactor(ff -> ff.field(SKU_SOURCE).modifier(FieldValueFactorModifier.Log1p).missing(1d))
                            .weight(4d)));
        }
    }

    // 安全关键字查询（不依赖自定义 analyzer 或子字段）
    public Query keywordSearchSafe(String keyword) {
        return QueryBuilders.match(m -> m.field(GOODS_NAME).query(keyword));
    }

    // 安全版查询构建（降级路径），避免使用 goodsName.IKS / 自定义 analyzer
    public NativeQueryBuilder createSafeSearchQueryBuilder(EsGoodsSearchDTO searchDTO, PageVO pageVo) {
        NativeQueryBuilder builder = NativeQuery.builder();
        if (pageVo != null) {
            int pageNumber = pageVo.getPageNumber() - 1;
            if (pageNumber < 0) {
                pageNumber = 0;
            }
            Pageable pageable = PageRequest.of(pageNumber, pageVo.getPageSize());
            builder.withPageable(pageable);
        }
        if (searchDTO != null) {
            List<Query> mustList = new ArrayList<>(this.commonSearch(searchDTO));
            mustList.add(QueryBuilders.match(a -> a.field("marketEnable").query(GoodsStatusEnum.UPPER.name())));
            mustList.add(QueryBuilders.match(a -> a.field("authFlag").query(GoodsAuthEnum.PASS.name())));

            if (CharSequenceUtil.isNotEmpty(searchDTO.getKeyword())) {
                mustList.add(this.keywordSearchSafe(searchDTO.getKeyword()));
            } else {
                // 不做 function score（涉及 IKS），保持最简单形态
            }

            builder.withQuery(q -> q.bool(b -> b.must(mustList)));

            if (pageVo != null && CharSequenceUtil.isNotEmpty(pageVo.getOrder()) && CharSequenceUtil.isNotEmpty(pageVo.getSort())) {
                builder.withSort(Sort.by(
                        Sort.Order.by(pageVo.getSort()).with(Sort.Direction.valueOf(pageVo.getOrder().toUpperCase()))));
            }
        }
        return builder;
    }

    // 超安全版查询构建（仅 match_all，不依赖任何字段/高亮/评分）
    public NativeQueryBuilder createSuperSafeSearchQueryBuilder(PageVO pageVo) {
        NativeQueryBuilder builder = NativeQuery.builder();
        if (pageVo != null) {
            int pageNumber = pageVo.getPageNumber() - 1;
            if (pageNumber < 0) {
                pageNumber = 0;
            }
            Pageable pageable = PageRequest.of(pageNumber, pageVo.getPageSize());
            builder.withPageable(pageable);
        }
        builder.withQuery(q -> q.matchAll(m -> m));
        return builder;
    }
}
