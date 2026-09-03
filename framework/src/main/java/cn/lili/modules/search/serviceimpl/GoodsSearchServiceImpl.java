package cn.lili.modules.search.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsParamsItemDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParamFacetRow;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.mapper.GoodsSearchParamMapper;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.goods.support.GoodsSearchParamParser;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import cn.lili.modules.search.entity.dos.GoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.GoodsSearchDTO;
import cn.lili.modules.search.entity.dto.ParamOptions;
import cn.lili.modules.search.entity.dto.SelectorOptions;
import cn.lili.modules.search.service.GoodsSearchService;
import com.alibaba.fastjson2.JSON;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * 商品搜索：列表走倒排 SQL 下推 + 真分页，selector 走三条全量聚合。
 */
@Service
public class GoodsSearchServiceImpl implements GoodsSearchService {

    private static final Map<String, SortColumn> SORT_WHITELIST = Map.of(
            "price", new SortColumn("gs.price", true),
            "buy_count", new SortColumn("IFNULL(gs.buy_count,0) + IFNULL(gs.virtual_sales,0)", false),
            "release_time", new SortColumn("g.create_time", true),
            "comment_num", new SortColumn("gs.comment_num", true)
    );

    private final GoodsSkuService goodsSkuService;

    @Autowired
    private GoodsSkuMapper goodsSkuMapper;

    @Autowired
    private GoodsSearchParamMapper goodsSearchParamMapper;

    @Autowired
    @Lazy
    private CategoryService categoryService;

    @Autowired
    @Lazy
    private PromotionService promotionService;

    @Autowired
    @Lazy
    private PromotionGoodsService promotionGoodsService;

    public GoodsSearchServiceImpl(GoodsSkuService goodsSkuService) {
        this.goodsSkuService = goodsSkuService;
    }

    @Override
    public Page<GoodsIndex> searchGoodsByPage(GoodsSearchDTO searchDTO, PageVO pageVo) {
        Map<String, List<String>> selectedProps = parseSelectedProps(searchDTO);
        Page<GoodsSkuDTO> skuPage = PageUtil.initPageWithoutOrder(pageVo);
        QueryWrapper<GoodsSkuDTO> queryWrapper = buildSharedWhere(searchDTO, selectedProps);
        applyOrderBy(queryWrapper, pageVo);
        IPage<GoodsSkuDTO> result = goodsSkuService.getGoodsSkuDTOByPage(skuPage, queryWrapper);
        Page<GoodsIndex> page = new Page<>(skuPage.getCurrent(), skuPage.getSize());
        if (result == null || CollUtil.isEmpty(result.getRecords())) {
            page.setTotal(result == null ? 0 : result.getTotal());
            page.setRecords(Collections.emptyList());
            return page;
        }
        page.setTotal(result.getTotal());
        List<GoodsIndex> records = result.getRecords().stream().map(this::toGoodsIndex).toList();
        records.forEach(this::fillPromotionInfo);
        page.setRecords(records);
        return page;
    }

    @Override
    public GoodsRelatedInfo getSelector(GoodsSearchDTO goodsSearch, PageVO pageVo) {
        Map<String, List<String>> selectedProps = parseSelectedProps(goodsSearch);
        GoodsRelatedInfo relatedInfo = new GoodsRelatedInfo();
        relatedInfo.setBrands(queryBrandSelector(goodsSearch, selectedProps));
        relatedInfo.setCategories(queryCategorySelector(goodsSearch, selectedProps));
        relatedInfo.setParamOptions(queryParamSelector(goodsSearch, selectedProps));
        return relatedInfo;
    }

    @Override
    public List<GoodsIndex> getGoodsBySkuIds(List<String> skuIds, PageVO pageVo) {
        if (CollUtil.isEmpty(skuIds)) {
            return Collections.emptyList();
        }
        GoodsSearchParams params = new GoodsSearchParams();
        params.setSkuIds(skuIds);
        return goodsSkuService.getGoodsSkuByList(params).stream().map(this::buildGoodsIndex).toList();
    }

    @Override
    public GoodsIndex getGoodsById(String id) {
        if (CharSequenceUtil.isEmpty(id)) {
            return null;
        }
        List<GoodsIndex> indexes = getGoodsBySkuIds(List.of(id), null);
        return indexes.isEmpty() ? null : indexes.get(0);
    }

    /**
     * selector 三条全量 SQL：各自新建 wrapper，共用 {@link #buildSharedWhere}，不使用列表 pageSize。
     */
    private List<SelectorOptions> queryBrandSelector(GoodsSearchDTO searchDTO, Map<String, List<String>> selectedProps) {
        QueryWrapper<GoodsSkuDTO> wrapper = buildSharedWhere(searchDTO, selectedProps);
        wrapper.ne("gs.brand_id", "0");
        List<GoodsSkuDTO> rows = goodsSkuMapper.selectSelectorBrands(wrapper);
        if (CollUtil.isEmpty(rows)) {
            return Collections.emptyList();
        }
        List<SelectorOptions> options = new ArrayList<>();
        for (GoodsSkuDTO row : rows) {
            if (row == null || CharSequenceUtil.isEmpty(row.getBrandId())) {
                continue;
            }
            SelectorOptions option = new SelectorOptions();
            option.setValue(row.getBrandId());
            option.setName(CharSequenceUtil.isNotEmpty(row.getBrandName()) ? row.getBrandName() : row.getBrandId());
            option.setUrl(row.getBrandLogo());
            options.add(option);
        }
        return options;
    }

    private List<SelectorOptions> queryCategorySelector(GoodsSearchDTO searchDTO, Map<String, List<String>> selectedProps) {
        QueryWrapper<GoodsSkuDTO> wrapper = buildSharedWhere(searchDTO, selectedProps);
        return resolveCategorySelector(goodsSkuMapper.selectSelectorCategoryPaths(wrapper));
    }

    /**
     * 批量解析分类名：一次 {@code listByIdsOrderByLevel}，再按各 path 自身的 id 顺序拼名。
     * allIds 为空时不发起查询，避免 MP 生成 {@code IN ()}。
     */
    private List<SelectorOptions> resolveCategorySelector(List<String> paths) {
        if (CollUtil.isEmpty(paths)) {
            return Collections.emptyList();
        }
        List<String> distinctPaths = paths.stream()
                .filter(CharSequenceUtil::isNotEmpty)
                .distinct()
                .toList();
        Set<String> allIds = distinctPaths.stream()
                .flatMap(p -> Arrays.stream(p.split(",")))
                .map(String::trim)
                .filter(CharSequenceUtil::isNotEmpty)
                .collect(Collectors.toSet());
        if (allIds.isEmpty()) {
            return Collections.emptyList();
        }
        Map<String, String> nameById = new HashMap<>();
        if (categoryService != null) {
            List<Category> categories = categoryService.listByIdsOrderByLevel(new ArrayList<>(allIds));
            if (CollUtil.isNotEmpty(categories)) {
                nameById = categories.stream()
                        .filter(c -> c != null && CharSequenceUtil.isNotEmpty(c.getId()))
                        .collect(Collectors.toMap(Category::getId, Category::getName, (a, b) -> a));
            }
        }
        List<SelectorOptions> options = new ArrayList<>();
        for (String path : distinctPaths) {
            SelectorOptions option = new SelectorOptions();
            option.setValue(path);
            String name = Arrays.stream(path.split(","))
                    .map(String::trim)
                    .filter(CharSequenceUtil::isNotEmpty)
                    .map(nameById::get)
                    .filter(CharSequenceUtil::isNotEmpty)
                    .collect(Collectors.joining(","));
            option.setName(CharSequenceUtil.isNotEmpty(name) ? name : path);
            options.add(option);
        }
        return options;
    }

    private List<ParamOptions> queryParamSelector(GoodsSearchDTO searchDTO, Map<String, List<String>> selectedProps) {
        QueryWrapper<GoodsSkuDTO> wrapper = buildSharedWhere(searchDTO, selectedProps);
        List<String> excludeNames = selectedProps == null || selectedProps.isEmpty()
                ? Collections.emptyList()
                : new ArrayList<>(selectedProps.keySet());
        List<GoodsSearchParamFacetRow> rows = goodsSearchParamMapper.selectSelectorParams(wrapper, excludeNames);
        return toParamOptions(rows);
    }

    /**
     * 按参数名分组；维度顺序：有 sort 的在前（取 MIN），sort 为 null 的排最后。
     */
    private List<ParamOptions> toParamOptions(List<GoodsSearchParamFacetRow> rows) {
        if (CollUtil.isEmpty(rows)) {
            return Collections.emptyList();
        }
        Map<String, Integer> minSortByName = new LinkedHashMap<>();
        Map<String, List<String>> valuesByName = new LinkedHashMap<>();
        for (GoodsSearchParamFacetRow row : rows) {
            if (row == null || CharSequenceUtil.isEmpty(row.getParamName()) || CharSequenceUtil.isEmpty(row.getParamValue())) {
                continue;
            }
            List<String> values = valuesByName.computeIfAbsent(row.getParamName(), key -> new ArrayList<>());
            if (!values.contains(row.getParamValue())) {
                values.add(row.getParamValue());
            }
            Integer rowSort = row.getSort();
            if (!minSortByName.containsKey(row.getParamName())) {
                minSortByName.put(row.getParamName(), rowSort);
            } else if (rowSort != null) {
                Integer current = minSortByName.get(row.getParamName());
                if (current == null || rowSort < current) {
                    minSortByName.put(row.getParamName(), rowSort);
                }
            }
        }
        List<String> names = new ArrayList<>(valuesByName.keySet());
        names.sort(Comparator
                .comparing((String name) -> minSortByName.get(name) == null)
                .thenComparing(minSortByName::get, Comparator.nullsLast(Integer::compareTo)));
        return names.stream().map(name -> {
            ParamOptions option = new ParamOptions();
            option.setKey(name);
            option.setValues(valuesByName.get(name));
            return option;
        }).toList();
    }

    /**
     * 只产出 WHERE 条件，绝不调用 last()/orderBy —— C4 要把它嵌进别的 SQL 形状里。
     * selectedProps 由调用方在请求入口解析一次后传入，本方法不再自行调用 parseSelectedProps。
     */
    private QueryWrapper<GoodsSkuDTO> buildSharedWhere(GoodsSearchDTO searchDTO,
                                                       Map<String, List<String>> selectedProps) {
        QueryWrapper<GoodsSkuDTO> queryWrapper = new QueryWrapper<>();
        applyBaseSkuConditions(queryWrapper, searchDTO);
        applyParamExists(queryWrapper, selectedProps);
        if (searchDTO != null && CharSequenceUtil.isNotEmpty(searchDTO.getBrandId())) {
            queryWrapper.in("gs.brand_id", List.of(searchDTO.getBrandId().split("@")));
        }
        queryWrapper.apply("(g.delete_flag IS NULL OR g.delete_flag = 0)");
        if (searchDTO != null) {
            queryWrapper.apply(CharSequenceUtil.isNotEmpty(searchDTO.getNeGoodsType()),
                    "(gs.goods_type IS NULL OR gs.goods_type <> {0})", searchDTO.getNeGoodsType());
            queryWrapper.apply(CharSequenceUtil.isNotEmpty(searchDTO.getNeSalesModel()),
                    "(gs.sales_model IS NULL OR gs.sales_model <> {0})", searchDTO.getNeSalesModel());
        }
        return queryWrapper;
    }

    /**
     * 仅列表路径调用，把白名单 ORDER BY 挂到 wrapper 上。
     * 排序输入只用于查表，永远不进 SQL 文本；不经过 SqlFilter.hit（buyCount 会误伤 COUNT）。
     */
    private void applyOrderBy(QueryWrapper<GoodsSkuDTO> wrapper, PageVO pageVo) {
        String sortKey = pageVo == null || CharSequenceUtil.isEmpty(pageVo.getSort())
                ? null : StringUtils.camel2Underline(pageVo.getSort());
        SortColumn column = sortKey == null ? null : SORT_WHITELIST.get(sortKey);
        if (column == null) {
            wrapper.last("ORDER BY gs.id DESC");
            return;
        }
        boolean desc = pageVo != null && "desc".equalsIgnoreCase(pageVo.getOrder());
        String direction = desc ? "DESC" : "ASC";
        StringBuilder orderBy = new StringBuilder("ORDER BY ");
        if (column.nullable()) {
            orderBy.append("(").append(column.expression()).append(" IS NULL), ");
        }
        orderBy.append(column.expression()).append(" ").append(direction)
                .append(", gs.id ").append(direction);
        wrapper.last(orderBy.toString());
    }

    private void applyBaseSkuConditions(QueryWrapper<GoodsSkuDTO> queryWrapper, GoodsSearchDTO searchDTO) {
        queryWrapper.eq("gs.market_enable", GoodsStatusEnum.UPPER.name());
        queryWrapper.eq("gs.auth_flag", GoodsAuthEnum.PASS.name());
        queryWrapper.eq("gs.delete_flag", false);
        if (searchDTO == null) {
            return;
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
    }

    private void applyParamExists(QueryWrapper<GoodsSkuDTO> queryWrapper, Map<String, List<String>> selectedProps) {
        if (selectedProps == null || selectedProps.isEmpty()) {
            return;
        }
        selectedProps.forEach((name, values) -> {
            if (CharSequenceUtil.isEmpty(name) || CollUtil.isEmpty(values)) {
                return;
            }
            String placeholders = IntStream.range(1, values.size() + 1)
                    .mapToObj(i -> "{" + i + "}").collect(Collectors.joining(", "));
            Object[] args = Stream.concat(Stream.of(name), values.stream()).toArray();
            queryWrapper.apply(
                    "EXISTS (SELECT 1 FROM li_goods_search_param p WHERE p.goods_id = gs.goods_id"
                            + " AND p.param_name = {0} AND p.param_value IN (" + placeholders + "))", args);
        });
    }

    private GoodsIndex toGoodsIndex(GoodsSkuDTO sku) {
        List<GoodsParamsItemDTO> params = parseIndexParams(sku.getParams());
        GoodsIndex index = new GoodsIndex(sku, params);
        if (!"0".equals(sku.getBrandId())) {
            index.setBrandName(sku.getBrandName());
            index.setBrandUrl(sku.getBrandLogo());
        }
        index.setReleaseTime(sku.getGoodsCreateTime());
        return index;
    }

    private GoodsIndex buildGoodsIndex(GoodsSku sku) {
        return fillPromotionInfo(new GoodsIndex(sku));
    }

    private GoodsIndex fillPromotionInfo(GoodsIndex index) {
        if (index == null) {
            return null;
        }
        Map<String, Object> promotionMap = buildPromotionMap(index);
        index.setPromotionMapJson(JSON.toJSONString(promotionMap));
        fillPromotionPrice(index);
        return index;
    }

    private Map<String, Object> buildPromotionMap(GoodsIndex index) {
        if (promotionService == null || index == null) {
            return Collections.emptyMap();
        }
        Map<String, Object> promotionMap = promotionService.getGoodsSkuPromotionMap(index.getStoreId(), index.getId());
        return promotionMap == null ? Collections.emptyMap() : promotionMap;
    }

    private void fillPromotionPrice(GoodsIndex index) {
        if (promotionGoodsService == null || index == null) {
            return;
        }
        Double promotionPrice = promotionGoodsService.getValidPromotionsGoodsPrice(index.getId(),
                Arrays.asList(PromotionTypeEnum.SECKILL.name(), PromotionTypeEnum.PINTUAN.name()));
        if (promotionPrice != null) {
            index.setPromotionPrice(promotionPrice);
        }
    }

    private List<GoodsParamsItemDTO> parseIndexParams(String params) {
        return GoodsSearchParamParser.parseIndexed(params);
    }

    Map<String, List<String>> parseSelectedProps(GoodsSearchDTO searchDTO) {
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
        searchDTO.getNotShowCol().put("name", new ArrayList<>(selectedProps.keySet()));
        searchDTO.getNotShowCol().put("value", selectedProps.values().stream().flatMap(List::stream).toList());
        return selectedProps;
    }

    private record SortColumn(String expression, boolean nullable) {
    }
}
