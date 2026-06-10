package cn.lili.modules.search.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.entity.dos.EsGoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import cn.lili.modules.search.entity.dto.SelectorOptions;
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

/**
 * all-in-one 精简模式不启用 Elasticsearch，搜索入口返回空结果以保证业务主流程可启动。
 */
@Service
@ConditionalOnProperty(value = "lili.data.elasticsearch.enabled", havingValue = "false")
public class EsGoodsSearchServiceNoop implements EsGoodsSearchService {

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
        relatedInfo.setParamOptions(Collections.emptyList());
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
        GoodsSearchParams params = buildMysqlSearchParams(searchDTO);
        List<GoodsSku> skuList = goodsSkuService.getGoodsSkuByList(params);
        if (CollUtil.isEmpty(skuList)) {
            return Collections.emptyList();
        }
        return skuList.stream()
                .filter(sku -> matchBrand(sku, searchDTO))
                .filter(sku -> matchExcludedGoodsType(sku, searchDTO))
                .filter(sku -> matchExcludedSalesModel(sku, searchDTO))
                .map(EsGoodsIndex::new)
                .toList();
    }

    private GoodsSearchParams buildMysqlSearchParams(EsGoodsSearchDTO searchDTO) {
        GoodsSearchParams params = new GoodsSearchParams();
        params.setMarketEnable(GoodsStatusEnum.UPPER.name());
        params.setAuthFlag(GoodsAuthEnum.PASS.name());
        if (searchDTO == null) {
            return params;
        }
        params.setGoodsName(searchDTO.getKeyword());
        params.setCategoryPath(searchDTO.getCategoryId());
        params.setStoreId(searchDTO.getStoreId());
        params.setStoreCategoryPath(searchDTO.getStoreCatId());
        params.setRecommend(searchDTO.getRecommend());
        params.setPrice(searchDTO.getPrice());
        params.setGoodsType(searchDTO.getGoodsType());
        params.setSalesModel(searchDTO.getSalesModel());
        if (CharSequenceUtil.isNotEmpty(searchDTO.getIds())) {
            params.setSkuIds(List.of(searchDTO.getIds().split(",")));
        }
        return params;
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
