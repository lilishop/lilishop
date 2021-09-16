package cn.lili.controller.other;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.thread.ThreadUtil;
import cn.hutool.core.util.ArrayUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Brand;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.BrandService;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.modules.store.entity.dos.StoreGoodsLabel;
import cn.lili.modules.store.service.StoreGoodsLabelService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import io.swagger.annotations.Api;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * ElasticsearchController
 *
 * @author Chopper
 * @version v1.0
 * 2021-03-24 18:32
 */
@RestController
@Api(tags = "ES初始化接口")
@RequestMapping("/manager/elasticsearch")
public class ElasticsearchController {

    @Autowired
    private EsGoodsIndexService esGoodsIndexService;

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    private GoodsService goodsService;

    @Autowired
    private StringRedisTemplate stringRedisTemplate;

    @Autowired
    private PromotionService promotionService;

    @Autowired
    private CategoryService categoryService;

    @Autowired
    private BrandService brandService;

    @Autowired
    private StoreGoodsLabelService storeGoodsLabelService;

    @Autowired
    private Cache cache;

    @GetMapping
    public ResultMessage<String> init() {

        Boolean flag = (Boolean) cache.get(CachePrefix.INIT_INDEX_FLAG.getPrefix());
        if (flag == null) {
            cache.put(CachePrefix.INIT_INDEX_FLAG.getPrefix(), false);
        }
        if (Boolean.TRUE.equals(flag)) {
            return ResultUtil.error(100000, "当前有任务在执行");
        }

        cache.put(CachePrefix.INIT_INDEX_PROCESS.getPrefix(), null);
        cache.put(CachePrefix.INIT_INDEX_FLAG.getPrefix(), true);
        ThreadUtil.execAsync(() -> {
            //查询商品信息
            LambdaQueryWrapper<GoodsSku> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.eq(GoodsSku::getIsAuth, GoodsAuthEnum.PASS.name());
            queryWrapper.eq(GoodsSku::getMarketEnable, GoodsStatusEnum.UPPER.name());

            List<GoodsSku> list = goodsSkuService.list(queryWrapper);
            List<EsGoodsIndex> esGoodsIndices = new ArrayList<>();
            //库存锁是在redis做的，所以生成索引，同时更新一下redis中的库存数量
            for (GoodsSku goodsSku : list) {
                Goods goods = goodsService.getById(goodsSku.getGoodsId());
                EsGoodsIndex index = new EsGoodsIndex(goodsSku);
                if (goods.getParams() != null && !goods.getParams().isEmpty()) {
                    List<GoodsParamsDTO> goodsParamDTOS = JSONUtil.toList(goods.getParams(), GoodsParamsDTO.class);
                    index = new EsGoodsIndex(goodsSku, goodsParamDTOS);
                }
                if (goods.getCategoryPath() != null) {
                    List<Category> categories = categoryService.listByIdsOrderByLevel(Arrays.asList(goods.getCategoryPath().split(",")));
                    if (!categories.isEmpty()) {
                        index.setCategoryNamePath(ArrayUtil.join(categories.stream().map(Category::getName).toArray(), ","));
                    }
                }
                Brand brand = brandService.getById(goods.getBrandId());
                if (brand != null) {
                    index.setBrandName(brand.getName());
                    index.setBrandUrl(brand.getLogo());
                }
                if (goods.getStoreCategoryPath() != null && CharSequenceUtil.isNotEmpty(goods.getStoreCategoryPath())) {
                    List<StoreGoodsLabel> storeGoodsLabels = storeGoodsLabelService.listByStoreIds(Arrays.asList(goods.getStoreCategoryPath().split(",")));
                    if (!storeGoodsLabels.isEmpty()) {
                        index.setStoreCategoryNamePath(ArrayUtil.join(storeGoodsLabels.stream().map(StoreGoodsLabel::getLabelName).toArray(), ","));
                    }
                }
                Map<String, Object> goodsCurrentPromotionMap = promotionService.getGoodsCurrentPromotionMap(index);
                index.setPromotionMap(goodsCurrentPromotionMap);
                esGoodsIndices.add(index);
                stringRedisTemplate.opsForValue().set(GoodsSkuService.getStockCacheKey(goodsSku.getId()), goodsSku.getQuantity().toString());
            }
            //初始化商品索引
            esGoodsIndexService.initIndex(esGoodsIndices);
        });
        return ResultUtil.success();
    }

    @GetMapping("/progress")
    public ResultMessage<Map<String, Integer>> getProgress() {
        try {
            Map<String, Integer> map = (Map<String, Integer>) cache.get(CachePrefix.INIT_INDEX_PROCESS.getPrefix());
            Boolean flag = (Boolean) cache.get(CachePrefix.INIT_INDEX_FLAG.getPrefix());
            map.put("flag", Boolean.TRUE.equals(flag) ? 1 : 0);
            return ResultUtil.data(map);
        } catch (Exception e) {
            return ResultUtil.data(null);
        }
    }
}
