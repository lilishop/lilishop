package cn.lili.modules.search.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsParamsItemDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.search.entity.dos.EsGoodsAttribute;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.entity.dos.EsGoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import cn.lili.modules.search.entity.dto.ParamOptions;
import cn.lili.modules.search.entity.dto.SelectorOptions;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import cn.lili.modules.search.service.EsGoodsSearchService;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.data.elasticsearch.client.elc.NativeQueryBuilder;
import org.springframework.data.elasticsearch.core.SearchPage;
import org.springframework.data.elasticsearch.core.query.Query;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * all-in-one 精简模式不启用 Elasticsearch，商品搜索降级为 MySQL 候选集 + 内存筛选。
 */
@Service
@ConditionalOnProperty(value = "lili.data.elasticsearch.enabled", havingValue = "false")
public class EsGoodsSearchServiceNoop implements EsGoodsSearchService {

    private static final long MYSQL_SEARCH_LIMIT = 5000L;

    private final GoodsSkuService goodsSkuService;

    public EsGoodsSearchServiceNoop(GoodsSkuService goodsSkuService) {
        this.goodsSkuService = goodsSkuService;
    }

    @Override
    public SearchPage<EsGoodsIndex> searchGoods(EsGoodsSearchDTO searchDTO, PageVO pageVo) {
        return null;
    }

    @Override
    public <T> SearchPage<T> searchGoods(Query searchQuery, Class<T> clazz) {
        return null;
    }

    @Override
    public Page<EsGoodsIndex> searchGoodsByPage(EsGoodsSearchDTO searchDTO, PageVO pageVo) {
        int pageNumber = getPageNumber(pageVo);
        int pageSize = getPageSize(pageVo);
        List<EsGoodsIndex> indexes = searchMysqlIndexes(searchDTO);
        sort(indexes, pageVo);

        Page<EsGoodsIndex> page = new Page<>(pageNumber, pageSize);
        page.setTotal(indexes.size());
        page.setRecords(slice(indexes, pageNumber, pageSize));
        return page;
    }

    @Override
    public EsGoodsRelatedInfo getSelector(EsGoodsSearchDTO goodsSearch, PageVO pageVo) {
        List<EsGoodsIndex> indexes = searchMysqlIndexes(goodsSearch);
        EsGoodsRelatedInfo relatedInfo = new EsGoodsRelatedInfo();
        relatedInfo.setCategories(buildSelector(indexes, EsGoodsIndex::getCategoryPath, EsGoodsIndex::getCategoryNamePath));
        relatedInfo.setBrands(buildSelector(indexes, EsGoodsIndex::getBrandId, EsGoodsIndex::getBrandName));
        relatedInfo.setParamOptions(buildParamOptions(indexes, goodsSearch));
        return relatedInfo;
    }

    @Override
    public List<EsGoodsIndex> getEsGoodsBySkuIds(List<String> skuIds, PageVO pageVo) {
        if (CollUtil.isEmpty(skuIds)) {
            return Collections.emptyList();
        }
        GoodsSearchParams params = new GoodsSearchParams();
        params.setSkuIds(skuIds);
        return goodsSkuService.getGoodsSkuByList(params).stream().map(EsGoodsIndex::new).toList();
    }

    @Override
    public EsGoodsIndex getEsGoodsById(String id) {
        if (CharSequenceUtil.isEmpty(id)) {
            return null;
        }
        List<EsGoodsIndex> indexes = getEsGoodsBySkuIds(List.of(id), null);
        return indexes.isEmpty() ? null : indexes.get(0);
    }

    @Override
    public NativeQueryBuilder createSearchQueryBuilder(EsGoodsSearchDTO searchDTO, PageVO pageVo) {
        return new NativeQueryBuilder();
    }

    private List<EsGoodsIndex> searchMysqlIndexes(EsGoodsSearchDTO searchDTO) {
        List<GoodsSkuDTO> skuList = queryMysqlSkuDTOList(searchDTO);
        if (CollUtil.isEmpty(skuList)) {
            return Collections.emptyList();
        }
        return skuList.stream()
                .filter(sku -> matchBrand(sku, searchDTO))
                .filter(sku -> matchExcludedGoodsType(sku, searchDTO))
                .filter(sku -> matchExcludedSalesModel(sku, searchDTO))
                .map(sku -> new EsGoodsIndex(sku, parseIndexParams(sku.getParams())))
                .filter(index -> matchProps(index, searchDTO))
                .toList();
    }

    private List<GoodsSkuDTO> queryMysqlSkuDTOList(EsGoodsSearchDTO searchDTO) {
        QueryWrapper<GoodsSkuDTO> queryWrapper = buildMysqlSkuDTOQuery(searchDTO);
        Page<GoodsSkuDTO> skuPage = new Page<>(1, MYSQL_SEARCH_LIMIT);
        IPage<GoodsSkuDTO> result = goodsSkuService.getGoodsSkuDTOByPage(skuPage, queryWrapper);
        if (result == null || CollUtil.isEmpty(result.getRecords())) {
            return Collections.emptyList();
        }
        return result.getRecords();
    }

    private QueryWrapper<GoodsSkuDTO> buildMysqlSkuDTOQuery(EsGoodsSearchDTO searchDTO) {
        QueryWrapper<GoodsSkuDTO> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("gs.market_enable", GoodsStatusEnum.UPPER.name());
        queryWrapper.eq("gs.auth_flag", GoodsAuthEnum.PASS.name());
        queryWrapper.eq("gs.delete_flag", false);
        if (searchDTO == null) {
            return queryWrapper;
        }
        if (CharSequenceUtil.isNotEmpty(searchDTO.getKeyword())) {
            queryWrapper.like("gs.goods_name", searchDTO.getKeyword());
        }
        if (CharSequenceUtil.isNotEmpty(searchDTO.getCategoryId())) {
            queryWrapper.like("gs.category_path", searchDTO.getCategoryId());
        }
        if (CharSequenceUtil.isNotEmpty(searchDTO.getStoreId())) {
            queryWrapper.eq("gs.store_id", searchDTO.getStoreId());
        }
        if (CharSequenceUtil.isNotEmpty(searchDTO.getStoreCatId())) {
            queryWrapper.like("gs.store_category_path", searchDTO.getStoreCatId());
        }
        if (searchDTO.getRecommend() != null) {
            queryWrapper.eq("gs.recommend", searchDTO.getRecommend());
        }
        if (CharSequenceUtil.isNotEmpty(searchDTO.getGoodsType())) {
            queryWrapper.eq("gs.goods_type", searchDTO.getGoodsType());
        }
        if (CharSequenceUtil.isNotEmpty(searchDTO.getSalesModel())) {
            queryWrapper.eq("gs.sales_model", searchDTO.getSalesModel());
        }
        if (CharSequenceUtil.isNotEmpty(searchDTO.getPrice())) {
            String[] price = searchDTO.getPrice().split("_");
            if (price.length > 1) {
                queryWrapper.between("gs.price", price[0], price[1]);
            } else {
                queryWrapper.ge("gs.price", price[0]);
            }
        }
        if (CharSequenceUtil.isNotEmpty(searchDTO.getIds())) {
            queryWrapper.in("gs.id", List.of(searchDTO.getIds().split(",")));
        }
        return queryWrapper;
    }

    private boolean matchBrand(GoodsSku sku, EsGoodsSearchDTO searchDTO) {
        if (searchDTO == null || CharSequenceUtil.isEmpty(searchDTO.getBrandId())) {
            return true;
        }
        return List.of(searchDTO.getBrandId().split("@")).contains(sku.getBrandId());
    }

    private boolean matchExcludedGoodsType(GoodsSku sku, EsGoodsSearchDTO searchDTO) {
        return searchDTO == null || CharSequenceUtil.isEmpty(searchDTO.getNeGoodsType()) || !Objects.equals(sku.getGoodsType(), searchDTO.getNeGoodsType());
    }

    private boolean matchExcludedSalesModel(GoodsSku sku, EsGoodsSearchDTO searchDTO) {
        return searchDTO == null || CharSequenceUtil.isEmpty(searchDTO.getNeSalesModel()) || !Objects.equals(sku.getSalesModel(), searchDTO.getNeSalesModel());
    }

    private List<GoodsParamsItemDTO> parseIndexParams(String params) {
        if (CharSequenceUtil.isEmpty(params)) {
            return Collections.emptyList();
        }
        try {
            List<GoodsParamsItemDTO> items = new ArrayList<>();
            JSONArray groups = JSON.parseArray(params);
            for (int i = 0; i < groups.size(); i++) {
                JSONObject group = groups.getJSONObject(i);
                JSONArray groupItems = group == null ? null : group.getJSONArray("goodsParamsItemDTOList");
                if (groupItems == null) {
                    continue;
                }
                for (int j = 0; j < groupItems.size(); j++) {
                    GoodsParamsItemDTO item = groupItems.getObject(j, GoodsParamsItemDTO.class);
                    if (item != null && Integer.valueOf(1).equals(item.getIsIndex())) {
                        items.add(item);
                    }
                }
            }
            return items;
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    private boolean matchProps(EsGoodsIndex index, EsGoodsSearchDTO searchDTO) {
        Map<String, List<String>> selectedProps = parseSelectedProps(searchDTO);
        if (selectedProps.isEmpty()) {
            return true;
        }
        if (CollUtil.isEmpty(index.getAttrList())) {
            return false;
        }
        for (Map.Entry<String, List<String>> entry : selectedProps.entrySet()) {
            for (String value : entry.getValue()) {
                boolean matched = index.getAttrList().stream().anyMatch(attr ->
                        Objects.equals(attr.getName(), entry.getKey()) && Objects.equals(attr.getValue(), value));
                if (!matched) {
                    return false;
                }
            }
        }
        return true;
    }

    private Map<String, List<String>> parseSelectedProps(EsGoodsSearchDTO searchDTO) {
        if (searchDTO == null || CharSequenceUtil.isEmpty(searchDTO.getProp())) {
            return Collections.emptyMap();
        }
        Map<String, List<String>> selectedProps = new LinkedHashMap<>();
        for (String prop : searchDTO.getProp().split("@")) {
            String[] pair = prop.split("_", 2);
            if (pair.length < 2 || CharSequenceUtil.isEmpty(pair[0]) || CharSequenceUtil.isEmpty(pair[1])) {
                continue;
            }
            selectedProps.computeIfAbsent(pair[0], key -> new ArrayList<>()).add(pair[1]);
        }
        // 与 ES 查询保持一致：已选参数名不再在参数聚合中重复展示。
        searchDTO.getNotShowCol().put("name", new ArrayList<>(selectedProps.keySet()));
        searchDTO.getNotShowCol().put("value", selectedProps.values().stream().flatMap(List::stream).toList());
        return selectedProps;
    }

    private List<ParamOptions> buildParamOptions(List<EsGoodsIndex> indexes, EsGoodsSearchDTO searchDTO) {
        if (CollUtil.isEmpty(indexes)) {
            return Collections.emptyList();
        }
        Set<String> selectedNames = searchDTO == null ? Collections.emptySet() :
                searchDTO.getNotShowCol().getOrDefault("name", Collections.emptyList()).stream().collect(Collectors.toSet());
        Map<String, List<String>> options = new LinkedHashMap<>();
        for (EsGoodsIndex index : indexes) {
            if (CollUtil.isEmpty(index.getAttrList())) {
                continue;
            }
            for (EsGoodsAttribute attr : index.getAttrList()) {
                if (selectedNames.contains(attr.getName()) || CharSequenceUtil.isEmpty(attr.getName()) || CharSequenceUtil.isEmpty(attr.getValue())) {
                    continue;
                }
                options.computeIfAbsent(attr.getName(), key -> new ArrayList<>());
                if (!options.get(attr.getName()).contains(attr.getValue())) {
                    options.get(attr.getName()).add(attr.getValue());
                }
            }
        }
        return options.entrySet().stream().map(entry -> {
            ParamOptions paramOptions = new ParamOptions();
            paramOptions.setKey(entry.getKey());
            paramOptions.setValues(entry.getValue());
            return paramOptions;
        }).toList();
    }

    private void sort(List<EsGoodsIndex> indexes, PageVO pageVo) {
        if (CollUtil.isEmpty(indexes) || pageVo == null || CharSequenceUtil.isEmpty(pageVo.getSort())) {
            return;
        }
        Comparator<EsGoodsIndex> comparator = switch (pageVo.getSort()) {
            case "price" -> Comparator.comparing(EsGoodsIndex::getPrice, Comparator.nullsLast(Double::compareTo));
            case "buyCount" -> Comparator.comparing(EsGoodsIndex::getBuyCount, Comparator.nullsLast(Integer::compareTo));
            case "releaseTime" -> Comparator.comparing(EsGoodsIndex::getReleaseTime, Comparator.nullsLast(Long::compareTo));
            default -> null;
        };
        if (comparator == null) {
            return;
        }
        if ("desc".equalsIgnoreCase(pageVo.getOrder())) {
            comparator = comparator.reversed();
        }
        indexes.sort(comparator);
    }

    private List<EsGoodsIndex> slice(List<EsGoodsIndex> indexes, int pageNumber, int pageSize) {
        int fromIndex = Math.max((pageNumber - 1) * pageSize, 0);
        if (fromIndex >= indexes.size()) {
            return Collections.emptyList();
        }
        int toIndex = Math.min(fromIndex + pageSize, indexes.size());
        return new ArrayList<>(indexes.subList(fromIndex, toIndex));
    }

    private List<SelectorOptions> buildSelector(List<EsGoodsIndex> indexes,
                                                java.util.function.Function<EsGoodsIndex, String> valueGetter,
                                                java.util.function.Function<EsGoodsIndex, String> titleGetter) {
        if (CollUtil.isEmpty(indexes)) {
            return Collections.emptyList();
        }
        Map<String, SelectorOptions> options = new LinkedHashMap<>();
        for (EsGoodsIndex index : indexes) {
            String value = valueGetter.apply(index);
            if (CharSequenceUtil.isEmpty(value)) {
                continue;
            }
            SelectorOptions option = new SelectorOptions();
            option.setValue(value);
            option.setName(CharSequenceUtil.isNotEmpty(titleGetter.apply(index)) ? titleGetter.apply(index) : value);
            options.putIfAbsent(value, option);
        }
        return new ArrayList<>(options.values());
    }

    private int getPageNumber(PageVO pageVo) {
        return pageVo == null || pageVo.getPageNumber() < 1 ? 1 : pageVo.getPageNumber();
    }

    private int getPageSize(PageVO pageVo) {
        return pageVo == null || pageVo.getPageSize() < 1 ? 10 : pageVo.getPageSize();
    }
}
