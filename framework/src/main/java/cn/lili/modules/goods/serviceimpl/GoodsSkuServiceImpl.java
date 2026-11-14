package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.NumberUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsGallery;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import cn.lili.modules.goods.entity.dto.GoodsSkuStockDTO;
import cn.lili.modules.stock.entity.dto.StockFlowContext;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.vos.GoodsSkuSpecVO;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.entity.vos.SpecValueVO;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.goods.service.GoodsGalleryService;
import cn.lili.modules.goods.support.GoodsVOAssembler;
import cn.lili.modules.goods.support.GoodsSkuReadSupport;
import cn.lili.modules.goods.support.GoodsSkuStatusSupport;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.goods.support.GoodsSkuSpecAssembler;
import cn.lili.modules.goods.support.GoodsSkuStockExcelSupport;
import cn.lili.modules.goods.support.GoodsSkuStockManager;
import cn.lili.modules.member.entity.dos.FootPrint;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.enums.CouponGetEnum;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import cn.lili.modules.search.service.GoodsIndexService;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 商品SKU业务层实现，复用基础读取和商品视图组件，不反向依赖完整商品业务服务。
 *
 * @author pikachu
 * @since 2020-02-23 15:18:56
 */
@Slf4j
@Service
public class GoodsSkuServiceImpl extends ServiceImpl<GoodsSkuMapper, GoodsSku> implements GoodsSkuService {

    @Autowired
    private Cache cache;

    @Autowired
    private MemberCouponService memberCouponService;

    @Autowired
    private GoodsGalleryService goodsGalleryService;

    @Autowired
    private LiliMessageTemplate liliMessageTemplate;

    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Autowired
    private GoodsVOAssembler goodsVOAssembler;

    @Autowired
    private GoodsSkuReadSupport skuReads;

    @Autowired
    private GoodsSkuStatusSupport skuStatus;

    @Autowired
    private GoodsIndexService goodsIndexService;

    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

    @Autowired
    private CouponService couponService;

    @Autowired
    private GoodsSkuStockExcelSupport goodsSkuStockExcelSupport;

    @Autowired
    private GoodsSkuSpecAssembler goodsSkuSpecAssembler;

    @Autowired
    private GoodsSkuStockManager goodsSkuStockManager;

    private GoodsSkuStockExcelSupport getGoodsSkuStockExcelSupport() {
        return this.goodsSkuStockExcelSupport != null ? this.goodsSkuStockExcelSupport : new GoodsSkuStockExcelSupport();
    }

    private GoodsSkuSpecAssembler getGoodsSkuSpecAssembler() {
        return this.goodsSkuSpecAssembler != null ? this.goodsSkuSpecAssembler : new GoodsSkuSpecAssembler();
    }

    private GoodsSkuStockManager getGoodsSkuStockManager() {
        return this.goodsSkuStockManager;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void add(Goods goods, GoodsOperationDTO goodsOperationDTO) {
        List<GoodsSku> skuList = goodsOperationDTO.getSkuList().stream().map(map -> {
            GoodsSku sku = JSON.parseObject(JSON.toJSONString(map), GoodsSku.class);
            sku.setGoodsId(goods.getId());
            return sku;
        }).collect(Collectors.toList());
        this.renderGoodsSkuList(skuList, goodsOperationDTO);
        for (GoodsSku sku : skuList) {
            this.applyECouponQuantityOnSave(sku);
            this.extendOldSkuValue(sku);
        }
        this.saveBatch(skuList);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(Goods goods, GoodsOperationDTO goodsOperationDTO) {
        List<GoodsSku> skuList = goodsOperationDTO.getSkuList().stream().map(map -> {
            GoodsSku sku = JSON.parseObject(JSON.toJSONString(map), GoodsSku.class);
            sku.setGoodsId(goods.getId());
            return sku;
        }).collect(Collectors.toList());
        List<GoodsSku> list = this.list(new LambdaQueryWrapper<GoodsSku>().eq(GoodsSku::getGoodsId, goods.getId()));
        List<GoodsSku> newSku = new ArrayList<>();
        List<GoodsSku> oldSku = new ArrayList<>();
        closePromotion(list, skuList);
        for (GoodsSku sku : skuList) {
            this.applyECouponQuantityOnSave(sku);
            this.extendOldSkuValue(sku);
            if (CharSequenceUtil.isEmpty(sku.getId())) {
                newSku.add(sku);
            } else {
                oldSku.add(sku);
            }
        }
        this.renderGoodsSkuList(newSku, goodsOperationDTO);
        this.renderGoodsSkuList(oldSku, goodsOperationDTO);
        if (!newSku.isEmpty()) {
            this.saveBatch(newSku);
        }
        if (!oldSku.isEmpty()) {
            this.updateBatchById(oldSku);
        }
    }

    private void closePromotion(List<GoodsSku> list, List<GoodsSku> skuList) {
        List<String> skuIds = skuList.stream().map(GoodsSku::getId).filter(CharSequenceUtil::isNotEmpty).collect(Collectors.toList());
        List<GoodsSku> deleteSkuList = list.stream().filter(i -> !skuIds.contains(i.getId())).collect(Collectors.toList());
        for (GoodsSku deleteSku : deleteSkuList) {
            promotionGoodsService.deletePromotionGoods(Collections.singletonList(deleteSku.getId()));
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(GoodsSku goodsSku) {
        this.updateById(goodsSku);
        cache.remove(GoodsSkuService.getCacheKeys(goodsSku.getId()));
        cache.put(GoodsSkuService.getCacheKeys(goodsSku.getId()), goodsSku);
    }

    @Override
    public void clearCache(String skuId) {
        cache.remove(GoodsSkuService.getCacheKeys(skuId));
        cache.remove(GoodsSkuService.getStockCacheKey(skuId));
    }

    private void clearStockRelatedCache(String skuId, String goodsId) {
        getGoodsSkuStockManager().clearStockRelatedCache(skuId, goodsId);
    }

    @Override
    public GoodsSku getGoodsSkuByIdFromCache(String id) {
        return skuReads.getGoodsSkuByIdFromCache(id);
    }

    @Override
    public GoodsSku getCanPromotionGoodsSkuByIdFromCache(String skuId) {
        return this.getGoodsSkuByIdFromCache(skuId);
    }

    @Override
    public Map<String, Object> getGoodsSkuDetail(String goodsId, String skuId) {
        Map<String, Object> map = new HashMap<>(16);
        GoodsVO goodsVO = goodsVOAssembler.getGoodsVO(goodsId);
        if (goodsVO == null) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        if (CharSequenceUtil.isEmpty(skuId) || "undefined".equals(skuId)) {
            skuId = goodsVO.getSkuList().get(0).getId();
        }
        GoodsSku goodsSku = this.getGoodsSkuByIdFromCache(skuId);
        if (goodsSku == null) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }

        if (GoodsStatusEnum.DOWN.name().equals(goodsVO.getMarketEnable()) || !GoodsAuthEnum.PASS.name().equals(goodsVO.getAuthFlag()) || Boolean.TRUE.equals(goodsVO.getDeleteFlag())) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }

        GoodsIndex goodsIndex = goodsIndexService.findById(skuId);
        if (goodsIndex == null) {
            goodsIndex = goodsIndexService.buildGoodsIndex(goodsSku);
        }

        GoodsSkuVO goodsSkuDetail = this.getGoodsSkuVO(goodsSku);

        Map<String, Object> promotionMap = goodsIndex == null ? Collections.emptyMap() : goodsIndex.getPromotionMap();
        AuthUser currentUser = UserContext.getCurrentUser();
        if (promotionMap != null && !promotionMap.isEmpty()) {
            promotionMap = promotionMap.entrySet().stream().parallel().filter(i -> {
                JSONObject jsonObject = JSON.parseObject(String.valueOf(i.getValue()));
                if (i.getKey().contains(PromotionTypeEnum.COUPON.name()) && currentUser != null) {
                    Integer couponLimitNum = jsonObject.getInteger("couponLimitNum");
                    Coupon coupon = couponService.getById(jsonObject.getString("id"));
                    if (coupon == null || (coupon.getPublishNum() != 0 && coupon.getReceivedNum() >= coupon.getPublishNum())) {
                        return false;
                    }
                    if (couponLimitNum > 0) {
                        Long count = memberCouponService.getMemberCouponNum(currentUser.getId(), jsonObject.getString("id"));
                        if (count >= couponLimitNum) {
                            return false;
                        }
                    }
                }
                return (jsonObject.get("getType") == null || CouponGetEnum.FREE.name().equals(jsonObject.getString("getType"))) && (jsonObject.get("startTime") != null && jsonObject.getObject("startTime", Date.class).getTime() <= System.currentTimeMillis()) && (jsonObject.get("endTime") == null || jsonObject.getObject("endTime", Date.class).getTime() >= System.currentTimeMillis());
            }).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

            Optional<Map.Entry<String, Object>> containsPromotion =
                    promotionMap.entrySet().stream().filter(i -> i.getKey().contains(PromotionTypeEnum.SECKILL.name()) || i.getKey().contains(PromotionTypeEnum.PINTUAN.name())).findFirst();
            if (containsPromotion.isPresent()) {
                JSONObject jsonObject = JSON.parseObject(String.valueOf(containsPromotion.get().getValue()));
                PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
                searchParams.setSkuId(skuId);
                searchParams.setPromotionId(jsonObject.getString("id"));
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
        if (goodsSkuDetail.getGoodsGalleryList() == null || goodsSkuDetail.getGoodsGalleryList().isEmpty()) {
            goodsSkuDetail.setGoodsGalleryList(goodsVO.getGoodsGalleryList());
        } else {
            goodsSkuDetail.getGoodsGalleryList().addAll(goodsVO.getGoodsGalleryList());
        }
        map.put("data", goodsSkuDetail);
        map.put("categoryName", resolveCategoryNames(goodsVO, goodsIndex));
        map.put("specs", this.groupBySkuAndSpec(goodsVO.getSkuList()));
        map.put("promotionMap", promotionMap);

        if (goodsVO.getGoodsParamsDTOList() != null && !goodsVO.getGoodsParamsDTOList().isEmpty()) {
            map.put("goodsParamsDTOList", goodsVO.getGoodsParamsDTOList());
        }

        if (currentUser != null) {
            FootPrint footPrint = new FootPrint(currentUser.getId(), goodsIndex.getStoreId(), goodsId, skuId);
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.VIEW_GOODS.name();
            asyncSendIfPresent(destination, footPrint);
        }
        return map;
    }

    private String[] resolveCategoryNames(GoodsVO goodsVO, GoodsIndex goodsIndex) {
        List<String> names = goodsVO != null ? goodsVO.getCategoryName() : null;
        if (names != null && !names.isEmpty()) {
            return names.toArray(new String[0]);
        }
        if (goodsIndex != null && CharSequenceUtil.isNotEmpty(goodsIndex.getCategoryNamePath())) {
            return goodsIndex.getCategoryNamePath().split(",");
        }
        return null;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsSkuStatus(Goods goods) {
        LambdaUpdateWrapper<GoodsSku> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(GoodsSku::getGoodsId, goods.getId());
        updateWrapper.set(GoodsSku::getMarketEnable, goods.getMarketEnable());
        updateWrapper.set(GoodsSku::getAuthFlag, goods.getAuthFlag());
        updateWrapper.set(GoodsSku::getDeleteFlag, goods.getDeleteFlag());
        this.update(updateWrapper);
        this.clearCacheByGoodsId(goods.getId());
    }

    private void clearCacheByGoodsId(String goodsId) {
        List<GoodsSku> goodsSkuList = this.list(new LambdaQueryWrapper<GoodsSku>().eq(GoodsSku::getGoodsId, goodsId));
        for (GoodsSku goodsSku : goodsSkuList) {
            this.clearCache(goodsSku.getId());
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsSkuStatusByStoreId(String storeId, String marketEnable, String authFlag) {
        skuStatus.updateByStoreId(storeId, marketEnable, authFlag);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsSkuStatusByGoodsIds(List<String> goodsIds, String marketEnable) {
        if (goodsIds == null || goodsIds.isEmpty()) {
            return;
        }
        LambdaUpdateWrapper<GoodsSku> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.in(GoodsSku::getGoodsId, goodsIds);
        updateWrapper.set(GoodsSku::getMarketEnable, marketEnable);
        this.update(updateWrapper);
        List<GoodsSku> list = this.list(new LambdaQueryWrapper<GoodsSku>().in(GoodsSku::getGoodsId, goodsIds));
        for (GoodsSku goodsSku : list) {
            this.clearCache(goodsSku.getId());
        }
    }

    @Override
    public List<GoodsSku> getGoodsSkuByIdFromCache(List<String> ids) {
        return skuReads.getGoodsSkuByIdFromCache(ids);
    }

    @Override
    public List<GoodsSkuVO> getGoodsListByGoodsId(String goodsId) {
        return skuReads.getGoodsListByGoodsId(goodsId);
    }

    @Override
    public List<GoodsSku> getGoodsSkuListByGoodsId(String goodsId) {
        return this.list(new LambdaQueryWrapper<GoodsSku>().eq(GoodsSku::getGoodsId, goodsId));
    }

    @Override
    public List<GoodsSku> listByIdsAndStoreId(List<String> skuIds, String storeId) {
        return this.list(new LambdaQueryWrapper<GoodsSku>()
                .in(GoodsSku::getId, skuIds)
                .eq(GoodsSku::getStoreId, storeId)
                .eq(GoodsSku::getDeleteFlag, false));
    }

    @Override
    public List<GoodsSku> listByGoodsIdAndStoreId(String goodsId, String storeId) {
        return this.list(new LambdaQueryWrapper<GoodsSku>()
                .eq(GoodsSku::getGoodsId, goodsId)
                .eq(GoodsSku::getStoreId, storeId)
                .eq(GoodsSku::getDeleteFlag, false));
    }

    @Override
    public List<GoodsSkuVO> getGoodsSkuVOList(List<GoodsSku> list) {
        return getGoodsSkuSpecAssembler().toVOList(list);
    }

    @Override
    public GoodsSkuVO getGoodsSkuVO(GoodsSku goodsSku) {
        return getGoodsSkuSpecAssembler().toVO(goodsSku);
    }

    @Override
    public IPage<GoodsSku> getGoodsSkuByPage(GoodsSearchParams searchParams) {
        return this.page(PageUtil.initPage(searchParams), searchParams.queryWrapper());
    }

    @Override
    public void queryExportStock(HttpServletResponse response, GoodsSearchParams searchParams) {
        getGoodsSkuStockExcelSupport().queryExportStock(response, searchParams);
    }

    @Override
    public void downloadStockTemplate(HttpServletResponse response) {
        getGoodsSkuStockExcelSupport().downloadStockTemplate(response);
    }

    @Override
    public void importStock(String storeId, MultipartFile file) {
        getGoodsSkuStockExcelSupport().importStock(storeId, file);
    }

    @Override
    public IPage<GoodsSkuDTO> getGoodsSkuDTOByPage(Page<GoodsSkuDTO> page, Wrapper<GoodsSkuDTO> queryWrapper) {
        return this.baseMapper.queryByParams(page, queryWrapper);
    }

    @Override
    public List<GoodsSku> getGoodsSkuByList(GoodsSearchParams searchParams) {
        return this.list(searchParams.queryWrapper());
    }

    @Override
    public void updateStocks(List<GoodsSkuStockDTO> goodsSkuStockDTOS) {
        getGoodsSkuStockManager().updateStocks(goodsSkuStockDTOS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStocks(List<GoodsSkuStockDTO> goodsSkuStockDTOS, StockFlowContext flowContext) {
        getGoodsSkuStockManager().updateStocks(goodsSkuStockDTOS, flowContext);
    }

    @Override
    public void updateStocksByType(List<GoodsSkuStockDTO> goodsSkuStockDTOS) {
        getGoodsSkuStockManager().updateStocksByType(goodsSkuStockDTOS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStocksByType(List<GoodsSkuStockDTO> goodsSkuStockDTOS, StockFlowContext flowContext) {
        getGoodsSkuStockManager().updateStocksByType(goodsSkuStockDTOS, flowContext);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateAlertQuantity(GoodsSkuStockDTO goodsSkuStockDTO) {
        getGoodsSkuStockManager().updateAlertQuantity(goodsSkuStockDTO);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void batchUpdateAlertQuantity(List<GoodsSkuStockDTO> goodsSkuStockDTOS) {
        getGoodsSkuStockManager().batchUpdateAlertQuantity(goodsSkuStockDTOS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStock(String skuId, Integer quantity) {
        getGoodsSkuStockManager().updateStock(skuId, quantity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStock(String skuId, Integer quantity, StockFlowContext flowContext) {
        getGoodsSkuStockManager().updateStock(skuId, quantity, flowContext);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStockFromCardKeySync(String skuId, Integer quantity) {
        getGoodsSkuStockManager().updateStockFromCardKeySync(skuId, quantity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStock(String skuId, Integer quantity, String type) {
        getGoodsSkuStockManager().updateStock(skuId, quantity, type);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStock(String skuId, Integer quantity, String type, StockFlowContext flowContext) {
        getGoodsSkuStockManager().updateStock(skuId, quantity, type, flowContext);
    }

    @Override
    public Integer getStock(String skuId) {
        return getGoodsSkuStockManager().getStock(skuId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsStock(List<GoodsSku> goodsSkus) {
        getGoodsSkuStockManager().updateGoodsStock(goodsSkus);
    }

    @Override
    public List<String> getSkuIdsByGoodsId(String goodsId) {
        return this.baseMapper.getGoodsSkuIdByGoodsId(goodsId);
    }

    @Override
    public boolean deleteAndInsertGoodsSkus(List<GoodsSku> goodsSkus) {
        return this.saveOrUpdateBatch(goodsSkus);
    }

    @Override
    public Long countSkuNum(String storeId) {
        return this.count(new LambdaQueryWrapper<GoodsSku>().eq(GoodsSku::getStoreId, storeId));
    }

    @Override
    public void renderGoodsSkuList(List<GoodsSku> goodsSkuList, GoodsOperationDTO goodsOperationDTO) {
        for (GoodsSku goodsSku : goodsSkuList) {
            extendOldSkuValue(goodsSku);
            this.renderImages(goodsSku, goodsOperationDTO.getGoodsGalleryList());
            applyECouponQuantityOnSave(goodsSku);
        }
    }

    @Override
    public void updateGoodsSkuBuyCount(String skuId, int buyCount) {
        this.update(new LambdaUpdateWrapper<GoodsSku>().eq(GoodsSku::getId, skuId).set(GoodsSku::getBuyCount, buyCount));
    }

    @Override
    public void updateGoodsSkuVirtualSales(String skuId, int virtualSales) {
        this.update(new LambdaUpdateWrapper<GoodsSku>()
                .eq(GoodsSku::getId, skuId)
                .set(GoodsSku::getVirtualSales, virtualSales));
        cache.remove(GoodsSkuService.getCacheKeys(skuId));
    }

    @Override
    public void batchUpdateGoodsSkuVirtualSales(List<String> skuIds, int virtualSales) {
        if (skuIds == null || skuIds.isEmpty()) {
            return;
        }
        this.update(new LambdaUpdateWrapper<GoodsSku>()
                .in(GoodsSku::getId, skuIds)
                .set(GoodsSku::getVirtualSales, virtualSales));
        skuIds.forEach(id -> cache.remove(GoodsSkuService.getCacheKeys(id)));
    }

    @Override
    public void updateGoodsSkuGrade(String goodsId, double grade, int commentNum) {
        this.update(new LambdaUpdateWrapper<GoodsSku>()
                .eq(GoodsSku::getGoodsId, goodsId)
                .set(GoodsSku::getGrade, grade)
                .set(GoodsSku::getCommentNum, commentNum));
    }

    @Override
    public Integer getGoodsStock(String goodsId) {
        return getGoodsSkuStockManager().getGoodsStock(goodsId);
    }

    @Override
    public Boolean freight(List<String> goodsId, String templateId) {
        return this.update(new LambdaUpdateWrapper<GoodsSku>()
                .in(GoodsSku::getGoodsId, goodsId)
                .set(GoodsSku::getFreightTemplateId, templateId));
    }

    private void applyECouponQuantityOnSave(GoodsSku goodsSku) {
        if (GoodsTypeEnum.E_COUPON.name().equals(goodsSku.getGoodsType())) {
            GoodsSku existing = CharSequenceUtil.isNotEmpty(goodsSku.getId()) ? this.getById(goodsSku.getId()) : null;
            goodsSku.setQuantity(existing != null && existing.getQuantity() != null ? existing.getQuantity() : 0);
        }
    }

    private void extendOldSkuValue(GoodsSku goodsSku) {
        if (CharSequenceUtil.isNotEmpty(goodsSku.getGoodsId())) {
            GoodsSku oldSku = this.getGoodsSkuByIdFromCache(goodsSku.getId());
            if (oldSku != null) {
                goodsSku.setCommentNum(oldSku.getCommentNum());
                goodsSku.setViewCount(oldSku.getViewCount());
                goodsSku.setBuyCount(oldSku.getBuyCount());
                goodsSku.setVirtualSales(oldSku.getVirtualSales());
                goodsSku.setGrade(oldSku.getGrade());
            }
        }
    }

    /**
     * 优先使用SKU规格图片，单规格未生成规格JSON或未配置规格图片时回退到商品主图。
     *
     * @param goodsSku 待填充图片的SKU
     * @param goodsImages 商品主图列表，无图片时保持原值
     */
    void renderImages(GoodsSku goodsSku, List<String> goodsImages) {
        if (goodsImages == null || goodsImages.isEmpty()) {
            return;
        }
        JSONObject jsonObject = JSON.parseObject(goodsSku.getSpecs());
        JSONArray imageArray = jsonObject == null ? null : jsonObject.getJSONArray("images");
        List<String> images = imageArray == null ? null : imageArray.toJavaList(String.class);
        GoodsGallery goodsGallery;
        if (images != null && !images.isEmpty()) {
            goodsGallery = goodsGalleryService.getGoodsGallery(images.get(0));
        } else {
            goodsGallery = goodsGalleryService.getGoodsGallery(goodsImages.get(0));
        }

        goodsSku.setBig(goodsGallery.getOriginal());
        goodsSku.setOriginal(goodsGallery.getOriginal());
        goodsSku.setThumbnail(goodsGallery.getThumbnail());
        goodsSku.setSmall(goodsGallery.getSmall());
    }

    private List<GoodsSkuSpecVO> groupBySkuAndSpec(List<GoodsSkuVO> goodsSkuVOList) {
        return getGoodsSkuSpecAssembler().groupBySkuAndSpec(goodsSkuVOList);
    }

    private void asyncSendIfPresent(String destination, Object payload) {
        if (liliMessageTemplate != null) {
            liliMessageTemplate.asyncSend(destination, payload, cn.lili.message.MessageSendCallbackBuilder.commonCallback());
        }
    }
}
