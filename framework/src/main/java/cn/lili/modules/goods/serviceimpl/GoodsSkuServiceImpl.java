package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.map.MapUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.NumberUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsGallery;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import cn.lili.modules.goods.entity.dto.GoodsSkuStockDTO;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.vos.GoodsSkuSpecVO;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.entity.vos.SpecValueVO;
import cn.lili.modules.goods.event.GeneratorEsGoodsIndexEvent;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.goods.service.*;
import cn.lili.modules.goods.sku.GoodsSkuBuilder;
import cn.lili.modules.goods.sku.render.SalesModelRender;
import cn.lili.modules.member.entity.dos.FootPrint;
import cn.lili.modules.member.entity.dto.EvaluationQueryParams;
import cn.lili.modules.member.entity.enums.EvaluationGradeEnum;
import cn.lili.modules.member.service.MemberEvaluationService;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.enums.CouponGetEnum;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.modules.search.utils.EsIndexUtil;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 商品sku业务层实现
 *
 * @author pikachu
 * @since 2020-02-23 15:18:56
 */
@Service
public class GoodsSkuServiceImpl extends ServiceImpl<GoodsSkuMapper, GoodsSku> implements GoodsSkuService {

    /**
     * 缓存
     */
    @Autowired
    private Cache cache;
    /**
     * 分类
     */
    @Autowired
    private CategoryService categoryService;
    /**
     * 商品相册
     */
    @Autowired
    private GoodsGalleryService goodsGalleryService;
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
    /**
     * 会员评价
     */
    @Autowired
    private MemberEvaluationService memberEvaluationService;
    /**
     * 商品
     */
    @Autowired
    private GoodsService goodsService;
    /**
     * 商品索引
     */
    @Autowired
    private EsGoodsIndexService goodsIndexService;

    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

    @Autowired
    private WholesaleService wholesaleService;

    @Autowired
    private List<SalesModelRender> salesModelRenders;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void add(Goods goods, GoodsOperationDTO goodsOperationDTO) {
        // 是否存在规格
        if (goodsOperationDTO.getSkuList() == null || goodsOperationDTO.getSkuList().isEmpty()) {
            throw new ServiceException(ResultCode.MUST_HAVE_GOODS_SKU);
        }
        // 检查是否需要生成索引
        List<GoodsSku> goodsSkus = GoodsSkuBuilder.buildBatch(goods, goodsOperationDTO);
        renderGoodsSkuList(goodsSkus, goodsOperationDTO);

        if (!goodsSkus.isEmpty()) {
            this.saveOrUpdateBatch(goodsSkus);
            this.updateStock(goodsSkus);
            this.generateEs(goods);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(Goods goods, GoodsOperationDTO goodsOperationDTO) {
        // 是否存在规格
        if (goodsOperationDTO.getSkuList() == null || goodsOperationDTO.getSkuList().isEmpty()) {
            throw new ServiceException(ResultCode.MUST_HAVE_GOODS_SKU);
        }
        List<GoodsSku> skuList;
        //删除旧的sku信息
        if (Boolean.TRUE.equals(goodsOperationDTO.getRegeneratorSkuFlag())) {
            skuList = GoodsSkuBuilder.buildBatch(goods, goodsOperationDTO);
            renderGoodsSkuList(skuList, goodsOperationDTO);
            List<GoodsSkuVO> goodsListByGoodsId = getGoodsListByGoodsId(goods.getId());
            List<String> oldSkuIds = new ArrayList<>();
            //删除旧索引
            for (GoodsSkuVO goodsSkuVO : goodsListByGoodsId) {
                oldSkuIds.add(goodsSkuVO.getId());
                cache.remove(GoodsSkuService.getCacheKeys(goodsSkuVO.getId()));
            }

            this.remove(new LambdaQueryWrapper<GoodsSku>().eq(GoodsSku::getGoodsId, goods.getId()));
            //删除sku相册
            goodsGalleryService.removeByGoodsId(goods.getId());

            //发送mq消息
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.SKU_DELETE.name();
            rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(oldSkuIds), RocketmqSendCallbackBuilder.commonCallback());
        } else {
            skuList = new ArrayList<>();
            for (Map<String, Object> map : goodsOperationDTO.getSkuList()) {
                GoodsSku sku = GoodsSkuBuilder.build(goods, map, goodsOperationDTO);
                renderGoodsSku(sku, goodsOperationDTO);
                skuList.add(sku);
                //如果商品状态值不对，则es索引移除
                if (goods.getAuthFlag().equals(GoodsAuthEnum.PASS.name()) && goods.getMarketEnable().equals(GoodsStatusEnum.UPPER.name())) {
                    goodsIndexService.deleteIndexById(sku.getId());
                    this.clearCache(sku.getId());
                }
            }
        }
        if (!skuList.isEmpty()) {
            this.remove(new LambdaQueryWrapper<GoodsSku>().eq(GoodsSku::getGoodsId, goods.getId()));
            this.saveOrUpdateBatch(skuList);
            this.updateStock(skuList);
            this.generateEs(goods);
        }
    }

    /**
     * 更新商品sku
     *
     * @param goodsSku sku信息
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(GoodsSku goodsSku) {
        this.updateById(goodsSku);
        cache.remove(GoodsSkuService.getCacheKeys(goodsSku.getId()));
        cache.put(GoodsSkuService.getCacheKeys(goodsSku.getId()), goodsSku);
    }


    /**
     * 清除sku缓存
     *
     * @param skuId skuID
     */
    @Override
    public void clearCache(String skuId) {
        cache.remove(GoodsSkuService.getCacheKeys(skuId));
    }

    @Override
    public GoodsSku getGoodsSkuByIdFromCache(String id) {
        //获取缓存中的sku
        GoodsSku goodsSku = (GoodsSku) cache.get(GoodsSkuService.getCacheKeys(id));
        //如果缓存中没有信息，则查询数据库，然后写入缓存
        if (goodsSku == null) {
            goodsSku = this.getById(id);
            if (goodsSku == null) {
                return null;
            }
            cache.put(GoodsSkuService.getCacheKeys(id), goodsSku);
        }

        //获取商品库存
        Integer integer = (Integer) cache.get(GoodsSkuService.getStockCacheKey(id));

        //库存不为空,库存与缓存中不一致
        if (integer != null && !goodsSku.getQuantity().equals(integer)) {
            //写入最新的库存信息
            goodsSku.setQuantity(integer);
            cache.put(GoodsSkuService.getCacheKeys(goodsSku.getId()), goodsSku);
        }
        return goodsSku;
    }

    @Override
    public Map<String, Object> getGoodsSkuDetail(String goodsId, String skuId) {
        Map<String, Object> map = new HashMap<>(16);
        //获取商品VO
        GoodsVO goodsVO = goodsService.getGoodsVO(goodsId);
        //如果skuid为空，则使用商品VO中sku信息获取
        if (CharSequenceUtil.isEmpty(skuId) || "undefined".equals(skuId)) {
            skuId = goodsVO.getSkuList().get(0).getId();
        }
        //从缓存拿商品Sku
        GoodsSku goodsSku = this.getGoodsSkuByIdFromCache(skuId);
        //如果使用商品ID无法查询SKU则返回错误
        if (goodsVO == null || goodsSku == null) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }

        //商品下架||商品未审核通过||商品删除，则提示：商品已下架
        if (GoodsStatusEnum.DOWN.name().equals(goodsVO.getMarketEnable()) || !GoodsAuthEnum.PASS.name().equals(goodsVO.getAuthFlag()) || Boolean.TRUE.equals(goodsVO.getDeleteFlag())) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }

        //获取当前商品的索引信息
        EsGoodsIndex goodsIndex = goodsIndexService.findById(skuId);
        if (goodsIndex == null) {
            goodsIndex = goodsIndexService.getResetEsGoodsIndex(goodsSku, goodsVO.getGoodsParamsDTOList());
        }

        //商品规格
        GoodsSkuVO goodsSkuDetail = this.getGoodsSkuVO(goodsSku);

        Map<String, Object> promotionMap = goodsIndex.getPromotionMap();
        //设置当前商品的促销价格
        if (promotionMap != null && !promotionMap.isEmpty()) {
            promotionMap = promotionMap.entrySet().stream().parallel().filter(i -> {
                JSONObject jsonObject = JSONUtil.parseObj(i.getValue());
                // 过滤活动赠送优惠券和无效时间的活动
                return (jsonObject.get("getType") == null || jsonObject.get("getType", String.class).equals(CouponGetEnum.FREE.name())) && (jsonObject.get("startTime") != null && jsonObject.get("startTime", Date.class).getTime() <= System.currentTimeMillis()) && (jsonObject.get("endTime") == null || jsonObject.get("endTime", Date.class).getTime() >= System.currentTimeMillis());
            }).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

            Optional<Map.Entry<String, Object>> containsPromotion = promotionMap.entrySet().stream().filter(i -> i.getKey().contains(PromotionTypeEnum.SECKILL.name()) || i.getKey().contains(PromotionTypeEnum.PINTUAN.name())).findFirst();
            if (containsPromotion.isPresent()) {
                JSONObject jsonObject = JSONUtil.parseObj(containsPromotion.get().getValue());
                PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
                searchParams.setSkuId(skuId);
                searchParams.setPromotionId(jsonObject.get("id").toString());
                PromotionGoods promotionsGoods = promotionGoodsService.getPromotionsGoods(searchParams);
                if (promotionsGoods != null && promotionsGoods.getPrice() != null) {
                    goodsSkuDetail.setPromotionFlag(true);
                    goodsSkuDetail.setPromotionPrice(promotionsGoods.getPrice());
                }
            } else {
                goodsSkuDetail.setPromotionFlag(false);
                goodsSkuDetail.setPromotionPrice(null);
            }

        }
        map.put("data", goodsSkuDetail);

        //获取分类
        String[] split = goodsSkuDetail.getCategoryPath().split(",");
        map.put("wholesaleList", wholesaleService.findByGoodsId(goodsSkuDetail.getGoodsId()));
        map.put("categoryName", categoryService.getCategoryNameByIds(Arrays.asList(split)));

        //获取规格信息
        map.put("specs", this.groupBySkuAndSpec(goodsVO.getSkuList()));
        map.put("promotionMap", promotionMap);

        //获取参数信息
        if (goodsVO.getGoodsParamsDTOList() != null && !goodsVO.getGoodsParamsDTOList().isEmpty()) {
            map.put("goodsParamsDTOList", goodsVO.getGoodsParamsDTOList());
        }

        //记录用户足迹
        if (UserContext.getCurrentUser() != null) {
            FootPrint footPrint = new FootPrint(UserContext.getCurrentUser().getId(), goodsId, skuId);
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.VIEW_GOODS.name();
            rocketMQTemplate.asyncSend(destination, footPrint, RocketmqSendCallbackBuilder.commonCallback());
        }
        return map;
    }

    /**
     * 更新商品sku状态
     *
     * @param goods 商品信息(Id,MarketEnable/AuthFlag)
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsSkuStatus(Goods goods) {
        LambdaUpdateWrapper<GoodsSku> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(CharSequenceUtil.isNotEmpty(goods.getId()), GoodsSku::getGoodsId, goods.getId());
        updateWrapper.eq(CharSequenceUtil.isNotEmpty(goods.getStoreId()), GoodsSku::getStoreId, goods.getStoreId());
        updateWrapper.set(GoodsSku::getMarketEnable, goods.getMarketEnable());
        updateWrapper.set(GoodsSku::getAuthFlag, goods.getAuthFlag());
        updateWrapper.set(GoodsSku::getDeleteFlag, goods.getDeleteFlag());
        boolean update = this.update(updateWrapper);
        if (Boolean.TRUE.equals(update)) {
            List<GoodsSku> goodsSkus = this.getGoodsSkuListByGoodsId(goods.getId());
            for (GoodsSku sku : goodsSkus) {
                cache.remove(GoodsSkuService.getCacheKeys(sku.getId()));
                cache.put(GoodsSkuService.getCacheKeys(sku.getId()), sku);
            }
            if (!goodsSkus.isEmpty()) {
                this.generateEs(goods);
            }
        }
    }

    /**
     * 更新商品sku状态根据店铺id
     *
     * @param storeId      店铺id
     * @param marketEnable 市场启用状态
     * @param authFlag     审核状态
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsSkuStatusByStoreId(String storeId, String marketEnable, String authFlag) {
        LambdaUpdateWrapper<GoodsSku> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(GoodsSku::getStoreId, storeId);
        updateWrapper.set(CharSequenceUtil.isNotEmpty(marketEnable), GoodsSku::getMarketEnable, marketEnable);
        updateWrapper.set(CharSequenceUtil.isNotEmpty(authFlag), GoodsSku::getAuthFlag, authFlag);
        boolean update = this.update(updateWrapper);
        if (Boolean.TRUE.equals(update)) {
            if (GoodsStatusEnum.UPPER.name().equals(marketEnable)) {
                applicationEventPublisher.publishEvent(new GeneratorEsGoodsIndexEvent("生成店铺商品", GoodsTagsEnum.GENERATOR_STORE_GOODS_INDEX.name(), storeId));
            } else if (GoodsStatusEnum.DOWN.name().equals(marketEnable)) {
                cache.vagueDel(CachePrefix.GOODS_SKU.getPrefix());
                applicationEventPublisher.publishEvent(new GeneratorEsGoodsIndexEvent("删除店铺商品", GoodsTagsEnum.STORE_GOODS_DELETE.name(), storeId));
            }
        }
    }

    @Override
    public List<GoodsSku> getGoodsSkuByIdFromCache(List<String> ids) {
        List<String> keys = new ArrayList<>();
        for (String id : ids) {
            keys.add(GoodsSkuService.getCacheKeys(id));
        }
        List<GoodsSku> list = cache.multiGet(keys);
        if (list == null || list.isEmpty()) {
            list = new ArrayList<>();
            List<GoodsSku> goodsSkus = listByIds(ids);
            for (GoodsSku skus : goodsSkus) {
                cache.put(GoodsSkuService.getCacheKeys(skus.getId()), skus);
                list.add(skus);
            }
        }
        return list;
    }

    @Override
    public List<GoodsSkuVO> getGoodsListByGoodsId(String goodsId) {
        List<GoodsSku> list = this.list(new LambdaQueryWrapper<GoodsSku>().eq(GoodsSku::getGoodsId, goodsId).orderByAsc(GoodsSku::getGoodsName));
        return this.getGoodsSkuVOList(list);
    }

    /**
     * 获取goodsId下所有的goodsSku
     *
     * @param goodsId 商品id
     * @return goodsSku列表
     */
    @Override
    public List<GoodsSku> getGoodsSkuListByGoodsId(String goodsId) {
        return this.list(new LambdaQueryWrapper<GoodsSku>().eq(GoodsSku::getGoodsId, goodsId));
    }

    @Override
    public List<GoodsSkuVO> getGoodsSkuVOList(List<GoodsSku> list) {
        List<GoodsSkuVO> goodsSkuVOS = new ArrayList<>();
        for (GoodsSku goodsSku : list) {
            GoodsSkuVO goodsSkuVO = this.getGoodsSkuVO(goodsSku);
            goodsSkuVOS.add(goodsSkuVO);
        }
        return goodsSkuVOS;
    }

    @Override
    public GoodsSkuVO getGoodsSkuVO(GoodsSku goodsSku) {
        //初始化商品
        GoodsSkuVO goodsSkuVO = new GoodsSkuVO(goodsSku);
        //获取sku信息
        JSONObject jsonObject = JSONUtil.parseObj(goodsSku.getSpecs());
        //用于接受sku信息
        List<SpecValueVO> specValueVOS = new ArrayList<>();
        //用于接受sku相册
        List<String> goodsGalleryList = new ArrayList<>();
        //循环提交的sku表单
        for (Map.Entry<String, Object> entry : jsonObject.entrySet()) {
            SpecValueVO specValueVO = new SpecValueVO();
            if ("images".equals(entry.getKey())) {
                specValueVO.setSpecName(entry.getKey());
                if (entry.getValue().toString().contains("url")) {
                    List<SpecValueVO.SpecImages> specImages = JSONUtil.toList(JSONUtil.parseArray(entry.getValue()), SpecValueVO.SpecImages.class);
                    specValueVO.setSpecImage(specImages);
                    goodsGalleryList = specImages.stream().map(SpecValueVO.SpecImages::getUrl).collect(Collectors.toList());
                }
            } else {
                specValueVO.setSpecName(entry.getKey());
                specValueVO.setSpecValue(entry.getValue().toString());
            }
            specValueVOS.add(specValueVO);
        }
        goodsSkuVO.setGoodsGalleryList(goodsGalleryList);
        goodsSkuVO.setSpecList(specValueVOS);
        return goodsSkuVO;
    }

    @Override
    public IPage<GoodsSku> getGoodsSkuByPage(GoodsSearchParams searchParams) {
        return this.page(PageUtil.initPage(searchParams), searchParams.queryWrapper());
    }

    @Override
    public IPage<GoodsSkuDTO> getGoodsSkuDTOByPage(Page<GoodsSkuDTO> page, Wrapper<GoodsSkuDTO> queryWrapper) {
        return this.baseMapper.queryByParams(page, queryWrapper);
    }

    /**
     * 列表查询商品sku信息
     *
     * @param searchParams 查询参数
     * @return 商品sku信息
     */
    @Override
    public List<GoodsSku> getGoodsSkuByList(GoodsSearchParams searchParams) {
        return this.list(searchParams.queryWrapper());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStocks(List<GoodsSkuStockDTO> goodsSkuStockDTOS) {
        for (GoodsSkuStockDTO goodsSkuStockDTO : goodsSkuStockDTOS) {
            this.updateStock(goodsSkuStockDTO.getSkuId(), goodsSkuStockDTO.getQuantity());
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStock(String skuId, Integer quantity) {
        GoodsSku goodsSku = getGoodsSkuByIdFromCache(skuId);
        if (goodsSku != null) {
            if (quantity <= 0) {
                goodsIndexService.deleteIndexById(goodsSku.getId());
            }
            goodsSku.setQuantity(quantity);
            boolean update = this.update(new LambdaUpdateWrapper<GoodsSku>().eq(GoodsSku::getId, skuId).set(GoodsSku::getQuantity, quantity));
            if (update) {
                cache.remove(CachePrefix.GOODS.getPrefix() + goodsSku.getGoodsId());
            }
            cache.put(GoodsSkuService.getCacheKeys(skuId), goodsSku);
            cache.put(GoodsSkuService.getStockCacheKey(skuId), quantity);

            //更新商品库存
            List<GoodsSku> goodsSkus = new ArrayList<>();
            goodsSkus.add(goodsSku);
            this.updateGoodsStuck(goodsSkus);
        }
    }

    @Override
    public Integer getStock(String skuId) {
        String cacheKeys = GoodsSkuService.getStockCacheKey(skuId);
        Integer stock = (Integer) cache.get(cacheKeys);
        if (stock != null) {
            return stock;
        } else {
            GoodsSku goodsSku = getGoodsSkuByIdFromCache(skuId);
            cache.put(cacheKeys, goodsSku.getQuantity());
            return goodsSku.getQuantity();
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsStuck(List<GoodsSku> goodsSkus) {
        Map<String, List<GoodsSku>> groupByGoodsIds = goodsSkus.stream().collect(Collectors.groupingBy(GoodsSku::getGoodsId));
        //获取相关的sku集合
        LambdaQueryWrapper<GoodsSku> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.in(GoodsSku::getGoodsId, groupByGoodsIds.keySet());
        List<GoodsSku> goodsSkuList = this.list(lambdaQueryWrapper);

        //统计每个商品的库存
        for (String goodsId : groupByGoodsIds.keySet()) {
            //库存
            Integer quantity = 0;
            for (GoodsSku goodsSku : goodsSkuList) {
                if (goodsId.equals(goodsSku.getGoodsId())) {
                    quantity += goodsSku.getQuantity();
                }
            }
            //保存商品库存结果
            goodsService.updateStock(goodsId, quantity);
        }


    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsSkuCommentNum(String skuId) {
        //获取商品信息
        GoodsSku goodsSku = this.getGoodsSkuByIdFromCache(skuId);

        EvaluationQueryParams queryParams = new EvaluationQueryParams();
        queryParams.setGrade(EvaluationGradeEnum.GOOD.name());
        queryParams.setSkuId(goodsSku.getId());
        //好评数量
        long highPraiseNum = memberEvaluationService.getEvaluationCount(queryParams);

        //更新商品评价数量
        goodsSku.setCommentNum(goodsSku.getCommentNum() != null ? goodsSku.getCommentNum() + 1 : 1);

        //好评率
        double grade = NumberUtil.mul(NumberUtil.div(highPraiseNum, goodsSku.getCommentNum().doubleValue(), 2), 100);
        goodsSku.setGrade(grade);
        //修改规格
        this.update(goodsSku);


        //修改规格索引,发送mq消息
        Map<String, Object> updateIndexFieldsMap = EsIndexUtil.getUpdateIndexFieldsMap(MapUtil.builder(new HashMap<String, Object>()).put("id", goodsSku.getId()).build(), MapUtil.builder(new HashMap<String, Object>()).put("commentNum", goodsSku.getCommentNum()).put("highPraiseNum", highPraiseNum).put("grade", grade).build());
        String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.UPDATE_GOODS_INDEX_FIELD.name();
        rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(updateIndexFieldsMap), RocketmqSendCallbackBuilder.commonCallback());

        //修改商品的评价数量
        goodsService.updateGoodsCommentNum(goodsSku.getGoodsId());
    }

    /**
     * 根据商品id获取全部skuId的集合
     *
     * @param goodsId goodsId
     * @return 全部skuId的集合
     */
    @Override
    public List<String> getSkuIdsByGoodsId(String goodsId) {
        return this.baseMapper.getGoodsSkuIdByGoodsId(goodsId);
    }

    /**
     * 发送生成ES商品索引
     *
     * @param goods 商品信息
     */
    @Override
    public void generateEs(Goods goods) {
        // 不生成没有审核通过且没有上架的商品
        if (!GoodsStatusEnum.UPPER.name().equals(goods.getMarketEnable()) || !GoodsAuthEnum.PASS.name().equals(goods.getAuthFlag())) {
            return;
        }
        applicationEventPublisher.publishEvent(new GeneratorEsGoodsIndexEvent("生成商品", GoodsTagsEnum.GENERATOR_GOODS_INDEX.name(), goods.getId()));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteAndInsertGoodsSkus(List<GoodsSku> goodsSkus) {
        int count = 0;
        for (GoodsSku skus : goodsSkus) {
            if (CharSequenceUtil.isEmpty(skus.getId())) {
                skus.setId(SnowFlake.getIdStr());
            }
            count = this.baseMapper.replaceGoodsSku(skus);
        }
        return count > 0;
    }

    @Override
    public Long countSkuNum(String storeId) {
        LambdaQueryWrapper<GoodsSku> queryWrapper = new LambdaQueryWrapper<>();

        queryWrapper
                .eq(GoodsSku::getStoreId, storeId)
                .eq(GoodsSku::getDeleteFlag, Boolean.FALSE)
                .eq(GoodsSku::getAuthFlag, GoodsAuthEnum.PASS.name())
                .eq(GoodsSku::getMarketEnable, GoodsStatusEnum.UPPER.name());
        return this.count(queryWrapper);
    }

    /**
     * 修改库存
     *
     * @param goodsSkus 商品SKU
     */
    private void updateStock(List<GoodsSku> goodsSkus) {
        //总库存数量
        Integer quantity = 0;
        for (GoodsSku sku : goodsSkus) {
            this.updateStock(sku.getId(), sku.getQuantity());
            quantity += sku.getQuantity();
        }
        //修改商品库存
        goodsService.updateStock(goodsSkus.get(0).getGoodsId(), quantity);
    }


    /**
     * 批量渲染商品sku
     *
     * @param goodsSkuList      sku集合
     * @param goodsOperationDTO 商品操作DTO
     */
    void renderGoodsSkuList(List<GoodsSku> goodsSkuList, GoodsOperationDTO goodsOperationDTO) {
        // 商品销售模式渲染器
        salesModelRenders.stream().filter(i -> i.getSalesMode().equals(goodsOperationDTO.getSalesModel())).findFirst().ifPresent(i -> i.renderBatch(goodsSkuList, goodsOperationDTO));
        for (GoodsSku goodsSku : goodsSkuList) {
            extendOldSkuValue(goodsSku);
            this.renderImages(goodsSku);
        }
    }

    /**
     * 渲染商品sku
     *
     * @param goodsSku          sku
     * @param goodsOperationDTO 商品操作DTO
     */
    void renderGoodsSku(GoodsSku goodsSku, GoodsOperationDTO goodsOperationDTO) {
        extendOldSkuValue(goodsSku);
        // 商品销售模式渲染器
        salesModelRenders.stream().filter(i -> i.getSalesMode().equals(goodsOperationDTO.getSalesModel())).findFirst().ifPresent(i -> i.renderSingle(goodsSku, goodsOperationDTO));
        this.renderImages(goodsSku);
    }

    /**
     * 将原sku的一些不会直接传递的值放到新的sku中
     *
     * @param goodsSku 商品sku
     */
    private void extendOldSkuValue(GoodsSku goodsSku) {
        if (CharSequenceUtil.isNotEmpty(goodsSku.getGoodsId())) {
            GoodsSku oldSku = this.getGoodsSkuByIdFromCache(goodsSku.getId());
            if (oldSku != null) {
                goodsSku.setCommentNum(oldSku.getCommentNum());
                goodsSku.setViewCount(oldSku.getViewCount());
                goodsSku.setBuyCount(oldSku.getBuyCount());
                goodsSku.setGrade(oldSku.getGrade());
            }
        }
    }

    /**
     * 渲染sku图片
     *
     * @param goodsSku sku
     */
    void renderImages(GoodsSku goodsSku) {
        JSONObject jsonObject = JSONUtil.parseObj(goodsSku.getSpecs());
        List<Map<String, String>> images = jsonObject.get("images", List.class);
        if (images != null && !images.isEmpty()) {
            GoodsGallery goodsGallery = goodsGalleryService.getGoodsGallery(images.get(0).get("url"));
            goodsSku.setBig(goodsGallery.getOriginal());
            goodsSku.setOriginal(goodsGallery.getOriginal());
            goodsSku.setThumbnail(goodsGallery.getThumbnail());
            goodsSku.setSmall(goodsGallery.getSmall());
        }
    }

    /**
     * 根据商品分组商品sku及其规格信息
     *
     * @param goodsSkuVOList 商品VO列表
     * @return 分组后的商品sku及其规格信息
     */
    private List<GoodsSkuSpecVO> groupBySkuAndSpec(List<GoodsSkuVO> goodsSkuVOList) {

        List<GoodsSkuSpecVO> skuSpecVOList = new ArrayList<>();
        for (GoodsSkuVO goodsSkuVO : goodsSkuVOList) {
            GoodsSkuSpecVO specVO = new GoodsSkuSpecVO();
            specVO.setSkuId(goodsSkuVO.getId());
            specVO.setSpecValues(goodsSkuVO.getSpecList());
            specVO.setQuantity(goodsSkuVO.getQuantity());
            skuSpecVOList.add(specVO);
        }
        return skuSpecVOList;
    }

}
