package cn.lili.modules.goods.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsSkuStockDTO;
import cn.lili.modules.goods.entity.enums.GoodsStockTypeEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.stock.entity.dto.StockFlowContext;
import cn.lili.modules.stock.entity.dto.StockFlowMessage;
import cn.lili.modules.stock.entity.enums.StockFlowDirectionEnum;
import cn.lili.modules.stock.event.StockFlowEvent;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 商品 SKU 库存与预警管理器，SKU持久化统一使用Mapper，不反向调用SKU服务
 * <p>
 * 封装 SKU 的库存原子增减、全量覆盖、卡密同步、预警线批量调整及库存台账事件发布，
 * 统一维护商品/SKU 多级缓存与促销库存联动。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
@NoArgsConstructor
public class GoodsSkuStockManager {

    @Autowired
    private GoodsSkuMapper goodsSkuMapper;

    @Autowired
    private GoodsStockAggregateSupport stockAggregates;

    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Autowired
    private Cache cache;

    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

    /**
     * 全参构造函数，用于非 Spring 托管环境及单元测试装配
     *
     * @param goodsSkuMapper            SKU 数据映射器
     * @param stockAggregates              商品库存汇总组件
     * @param promotionGoodsService     促销商品服务
     * @param cache                     缓存客户端
     * @param applicationEventPublisher 领域事件发布器
     */
    public GoodsSkuStockManager(GoodsSkuMapper goodsSkuMapper,
                                GoodsStockAggregateSupport stockAggregates,
                                PromotionGoodsService promotionGoodsService,
                                Cache cache,
                                ApplicationEventPublisher applicationEventPublisher) {
        this.goodsSkuMapper = goodsSkuMapper;
        this.stockAggregates = stockAggregates;
        this.promotionGoodsService = promotionGoodsService;
        this.cache = cache;
        this.applicationEventPublisher = applicationEventPublisher;
    }

    /**
     * 批量直接覆盖更新 SKU 目标库存（默认不记录业务流水）
     *
     * @param goodsSkuStockDTOS SKU 库存修改列表
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateStocks(List<GoodsSkuStockDTO> goodsSkuStockDTOS) {
        this.updateStocks(goodsSkuStockDTOS, null);
    }

    /**
     * 批量直接覆盖更新 SKU 目标库存并记录库存台账流水
     *
     * @param goodsSkuStockDTOS SKU 库存修改列表
     * @param flowContext       库存流水业务上下文
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateStocks(List<GoodsSkuStockDTO> goodsSkuStockDTOS, StockFlowContext flowContext) {
        if (goodsSkuStockDTOS == null || goodsSkuStockDTOS.isEmpty()) {
            return;
        }
        List<String> skuIds = goodsSkuStockDTOS.stream().map(GoodsSkuStockDTO::getSkuId).collect(Collectors.toList());
        List<GoodsSkuStockDTO> goodsSkuStockList = this.goodsSkuMapper.queryStocks(GoodsSearchParams.builder().ids(skuIds).build().queryWrapper());
        Map<String, List<GoodsSkuStockDTO>> groupByGoodsIds = goodsSkuStockList.stream().collect(Collectors.groupingBy(GoodsSkuStockDTO::getGoodsId));

        for (Map.Entry<String, List<GoodsSkuStockDTO>> entry : groupByGoodsIds.entrySet()) {
            for (GoodsSkuStockDTO goodsSku : entry.getValue()) {
                goodsSkuStockDTOS.stream()
                        .filter(i -> i.getSkuId().equals(goodsSku.getSkuId()))
                        .findFirst()
                        .ifPresent(i -> goodsSku.setQuantity(i.getQuantity()));

                this.updateStock(goodsSku.getSkuId(), goodsSku.getQuantity(), flowContext);
            }
            if (this.stockAggregates != null) {
                this.stockAggregates.updateStock(entry.getKey());
            }
        }
    }

    /**
     * 批量按增减方向原子更新 SKU 库存（默认不记录业务流水）
     *
     * @param goodsSkuStockDTOS SKU 库存修改列表
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateStocksByType(List<GoodsSkuStockDTO> goodsSkuStockDTOS) {
        this.updateStocksByType(goodsSkuStockDTOS, null);
    }

    /**
     * 批量按增减方向原子更新 SKU 库存并记录库存台账流水
     *
     * @param goodsSkuStockDTOS SKU 库存修改列表
     * @param flowContext       库存流水业务上下文
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateStocksByType(List<GoodsSkuStockDTO> goodsSkuStockDTOS, StockFlowContext flowContext) {
        if (goodsSkuStockDTOS == null || goodsSkuStockDTOS.isEmpty()) {
            return;
        }

        List<String> goodsIds = goodsSkuStockDTOS.stream()
                .map(GoodsSkuStockDTO::getGoodsId)
                .filter(CharSequenceUtil::isNotEmpty)
                .distinct()
                .collect(Collectors.toList());

        for (GoodsSkuStockDTO goodsSkuStockDTO : goodsSkuStockDTOS) {
            this.updateStock(goodsSkuStockDTO.getSkuId(), goodsSkuStockDTO.getQuantity(), goodsSkuStockDTO.getType(), flowContext);
        }
        if (this.stockAggregates != null) {
            for (String goodsId : goodsIds) {
                this.stockAggregates.updateStock(goodsId);
            }
        }
    }

    /**
     * 更新单个 SKU 预警库存数值并清理相关商品缓存
     *
     * @param goodsSkuStockDTO SKU 预警设置对象
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateAlertQuantity(GoodsSkuStockDTO goodsSkuStockDTO) {
        GoodsSku goodsSku = this.goodsSkuMapper.selectById(goodsSkuStockDTO.getSkuId());
        if (goodsSku == null) {
            return;
        }
        goodsSku.setAlertQuantity(goodsSkuStockDTO.getAlertQuantity());
        if (cache != null) {
            cache.remove(CachePrefix.GOODS.getPrefix() + goodsSku.getGoodsId());
        }
        this.goodsSkuMapper.updateById(goodsSku);
        // 保留原SKU服务更新时的缓存替换，避免预警值更新后读到旧SKU。
        if (cache != null) {
            cache.remove(GoodsSkuService.getCacheKeys(goodsSku.getId()));
            cache.put(GoodsSkuService.getCacheKeys(goodsSku.getId()), goodsSku);
        }
    }

    /**
     * 批量更新SKU预警库存数值并清理商品缓存，Mapper批量写入在同一事务中完成。
     *
     * @param goodsSkuStockDTOS SKU 预警设置列表
     */
    @Transactional(rollbackFor = Exception.class)
    public void batchUpdateAlertQuantity(List<GoodsSkuStockDTO> goodsSkuStockDTOS) {
        if (goodsSkuStockDTOS == null || goodsSkuStockDTOS.isEmpty()) {
            return;
        }
        List<String> skuIds = goodsSkuStockDTOS.stream().map(GoodsSkuStockDTO::getSkuId).collect(Collectors.toList());
        List<GoodsSkuStockDTO> goodsSkuStockList = this.goodsSkuMapper.queryStocks(GoodsSearchParams.builder().ids(skuIds).build().queryWrapper());
        List<String> goodsIdList = goodsSkuStockList.stream().map(GoodsSkuStockDTO::getGoodsId).collect(Collectors.toList());
        HashSet<String> uniqueSet = new HashSet<>(goodsIdList);

        if (cache != null) {
            for (String goodsId : uniqueSet) {
                cache.remove(CachePrefix.GOODS.getPrefix() + goodsId);
            }
        }

        List<GoodsSku> goodsSkuList = new ArrayList<>();
        for (GoodsSkuStockDTO goodsSkuStockDTO : goodsSkuStockDTOS) {
            GoodsSku goodsSku = this.goodsSkuMapper.selectById(goodsSkuStockDTO.getSkuId());
            if (goodsSku != null) {
                goodsSku.setAlertQuantity(goodsSkuStockDTO.getAlertQuantity());
                goodsSkuList.add(goodsSku);
            }
        }
        if (!goodsSkuList.isEmpty()) {
            this.goodsSkuMapper.updateById(goodsSkuList);
        }
    }

    /**
     * 单个直接覆盖更新 SKU 目标库存（默认不记录业务流水）
     *
     * @param skuId    商品 SKU ID
     * @param quantity 目标库存数量
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateStock(String skuId, Integer quantity) {
        this.updateStock(skuId, quantity, (StockFlowContext) null);
    }

    /**
     * 单个直接覆盖更新 SKU 目标库存并记录库存台账流水
     *
     * @param skuId       商品 SKU ID
     * @param quantity    目标库存数量
     * @param flowContext 库存流水业务上下文
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateStock(String skuId, Integer quantity, StockFlowContext flowContext) {
        if (CharSequenceUtil.isEmpty(skuId) || quantity == null || quantity < 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        GoodsSku goodsSku = loadGoodsSkuForStockUpdate(skuId);
        assertManualStockUpdateAllowed(goodsSku);
        Integer beforeStock = goodsSku.getQuantity() != null ? goodsSku.getQuantity() : 0;
        applyStockQuantityUpdate(goodsSku, skuId, quantity);

        if (flowContext != null && flowContext.getFlowType() != null && applicationEventPublisher != null) {
            int diff = quantity - beforeStock;
            if (diff != 0) {
                StockFlowDirectionEnum direction = diff > 0 ? StockFlowDirectionEnum.IN : StockFlowDirectionEnum.OUT;
                StockFlowMessage message = StockFlowMessage.builder()
                        .storeId(goodsSku.getStoreId())
                        .goodsId(goodsSku.getGoodsId())
                        .skuId(skuId)
                        .goodsName(goodsSku.getGoodsName())
                        .specs(goodsSku.getSpecs())
                        .flowType(flowContext.getFlowType().name())
                        .direction(direction.name())
                        .quantity(Math.abs(diff))
                        .beforeStock(beforeStock)
                        .afterStock(quantity)
                        .bizSn(flowContext.getBizSn())
                        .operator(flowContext.getOperator())
                        .remark(flowContext.getRemark())
                        .build();
                applicationEventPublisher.publishEvent(new StockFlowEvent(this, message));
            }
        }
    }

    /**
     * 卡密池同步专用：更新 E_COUPON SKU 的 quantity（绕过普通库存手工修改限制）
     *
     * @param skuId    商品 SKU ID
     * @param quantity 卡密池有效卡密总数
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateStockFromCardKeySync(String skuId, Integer quantity) {
        if (CharSequenceUtil.isEmpty(skuId) || quantity == null || quantity < 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        GoodsSku goodsSku = loadGoodsSkuForStockUpdate(skuId);
        applyStockQuantityUpdate(goodsSku, skuId, quantity);
    }

    /**
     * 单个按增减方向原子更新 SKU 库存（默认不记录业务流水）
     *
     * @param skuId    商品 SKU ID
     * @param quantity 变动库存数量
     * @param type     变动方向类型（ADD/SUB）
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateStock(String skuId, Integer quantity, String type) {
        this.updateStock(skuId, quantity, type, null);
    }

    /**
     * 单个按增减方向原子更新 SKU 库存并记录库存台账流水
     *
     * @param skuId       商品 SKU ID
     * @param quantity    变动库存数量
     * @param type        变动方向类型（ADD/SUB）
     * @param flowContext 库存流水业务上下文
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateStock(String skuId, Integer quantity, String type, StockFlowContext flowContext) {
        if (CharSequenceUtil.isEmpty(skuId) || quantity == null || quantity <= 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        if (!GoodsStockTypeEnum.ADD.name().equals(type) && !GoodsStockTypeEnum.SUB.name().equals(type)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        GoodsSku goodsSku = loadGoodsSkuForStockUpdate(skuId);
        assertManualStockUpdateAllowed(goodsSku);

        int rows = GoodsStockTypeEnum.ADD.name().equals(type)
                ? this.goodsSkuMapper.increaseStock(skuId, quantity)
                : this.goodsSkuMapper.reduceStock(skuId, quantity);
        if (rows == 0) {
            throw new ServiceException(ResultCode.GOODS_STOCK_INSUFFICIENT);
        }

        GoodsSku latestSku = this.goodsSkuMapper.selectById(skuId);
        Integer newQuantity = latestSku != null ? latestSku.getQuantity() : 0;
        clearStockRelatedCache(skuId, goodsSku.getGoodsId());
        if (this.promotionGoodsService != null) {
            this.promotionGoodsService.updatePromotionGoodsStock(goodsSku.getId(), newQuantity);
        }

        if (flowContext != null && flowContext.getFlowType() != null && applicationEventPublisher != null) {
            boolean isSub = GoodsStockTypeEnum.SUB.name().equals(type);
            Integer afterStock = newQuantity != null ? newQuantity : 0;
            Integer beforeStock = isSub ? afterStock + quantity : afterStock - quantity;
            StockFlowDirectionEnum direction = isSub ? StockFlowDirectionEnum.OUT : StockFlowDirectionEnum.IN;

            StockFlowMessage message = StockFlowMessage.builder()
                    .storeId(goodsSku.getStoreId())
                    .goodsId(goodsSku.getGoodsId())
                    .skuId(skuId)
                    .goodsName(goodsSku.getGoodsName())
                    .specs(goodsSku.getSpecs())
                    .flowType(flowContext.getFlowType().name())
                    .direction(direction.name())
                    .quantity(quantity)
                    .beforeStock(beforeStock)
                    .afterStock(afterStock)
                    .bizSn(flowContext.getBizSn())
                    .operator(flowContext.getOperator())
                    .remark(flowContext.getRemark())
                    .build();
            applicationEventPublisher.publishEvent(new StockFlowEvent(this, message));
        }
    }

    /**
     * 获取单个 SKU 的当前库存，优先从缓存获取
     *
     * @param skuId 商品 SKU ID
     * @return 当前可用库存
     */
    public Integer getStock(String skuId) {
        String cacheKeys = GoodsSkuService.getStockCacheKey(skuId);
        if (cache != null) {
            Integer stock = (Integer) cache.get(cacheKeys);
            if (stock != null) {
                return stock;
            }
        }
        GoodsSku goodsSku = loadGoodsSkuForStockUpdate(skuId);
        if (cache != null && goodsSku != null) {
            cache.put(cacheKeys, goodsSku.getQuantity());
        }
        return goodsSku != null ? goodsSku.getQuantity() : 0;
    }

    /**
     * 批量更新已有的 SKU 库存，并同步更新所属 SPU 总库存
     *
     * @param goodsSkus 待更新的 SKU 列表
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsStock(List<GoodsSku> goodsSkus) {
        if (goodsSkus == null || goodsSkus.isEmpty()) {
            return;
        }
        Map<String, List<GoodsSku>> groupByGoodsIds = goodsSkus.stream()
                .collect(Collectors.groupingBy(GoodsSku::getGoodsId));
        for (Map.Entry<String, List<GoodsSku>> entry : groupByGoodsIds.entrySet()) {
            for (GoodsSku goodsSku : entry.getValue()) {
                int quantity = goodsSku.getQuantity() == null ? 0 : goodsSku.getQuantity();
                if (GoodsTypeEnum.E_COUPON.name().equals(goodsSku.getGoodsType())) {
                    this.updateStockFromCardKeySync(goodsSku.getId(), quantity);
                } else {
                    this.updateStock(goodsSku.getId(), quantity);
                }
            }
            if (this.stockAggregates != null) {
                this.stockAggregates.updateStock(entry.getKey());
            }
        }
    }

    /**
     * 汇总指定商品 ID 下所有 SKU 的总库存
     *
     * @param goodsId 商品 ID
     * @return 该商品下的总库存量
     */
    public Integer getGoodsStock(String goodsId) {
        return stockAggregates.getGoodsStock(goodsId);
    }

    /**
     * 清理 SKU 库存相关的 Redis 缓存
     *
     * @param skuId   SKU ID
     * @param goodsId 商品 ID
     */
    public void clearStockRelatedCache(String skuId, String goodsId) {
        if (cache != null) {
            cache.remove(GoodsSkuService.getCacheKeys(skuId));
            cache.remove(GoodsSkuService.getStockCacheKey(skuId));
            if (CharSequenceUtil.isNotEmpty(goodsId)) {
                cache.remove(CachePrefix.GOODS.getPrefix() + goodsId);
            }
        }
    }

    /**
     * 加载用于库存更新的SKU，沿用先查缓存、未命中再查Mapper并回填缓存的规则
     *
     * @param skuId 商品 SKU ID
     * @return SKU 实体
     */
    private GoodsSku loadGoodsSkuForStockUpdate(String skuId) {
        GoodsSku goodsSku = cache == null ? null : (GoodsSku) cache.get(GoodsSkuService.getCacheKeys(skuId));
        if (goodsSku == null) {
            goodsSku = this.goodsSkuMapper.selectById(skuId);
            if (goodsSku != null && cache != null) {
                cache.put(GoodsSkuService.getCacheKeys(skuId), goodsSku);
            }
        }
        if (goodsSku == null) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        return goodsSku;
    }

    /**
     * 校验是否允许手动更新库存，电子卡券由卡密池驱动禁止人工手动修改
     *
     * @param goodsSku SKU 实体
     */
    private void assertManualStockUpdateAllowed(GoodsSku goodsSku) {
        if (goodsSku != null && GoodsTypeEnum.E_COUPON.name().equals(goodsSku.getGoodsType())) {
            throw new ServiceException(ResultCode.CARD_KEY_STOCK_MANUAL_FORBIDDEN);
        }
    }

    /**
     * 执行 SKU 库存数量的持久化写入与多级缓存刷新
     *
     * @param goodsSku 目标 SKU
     * @param skuId    SKU ID
     * @param quantity 目标数量
     */
    private void applyStockQuantityUpdate(GoodsSku goodsSku, String skuId, Integer quantity) {
        goodsSku.setQuantity(quantity);
        int rows = this.goodsSkuMapper.update(null,
                new LambdaUpdateWrapper<GoodsSku>().eq(GoodsSku::getId, skuId).set(GoodsSku::getQuantity, quantity));
        if (rows > 0) {
            clearStockRelatedCache(skuId, goodsSku.getGoodsId());
            if (cache != null) {
                cache.put(GoodsSkuService.getCacheKeys(skuId), goodsSku);
                cache.put(GoodsSkuService.getStockCacheKey(skuId), quantity);
            }
            if (this.promotionGoodsService != null) {
                this.promotionGoodsService.updatePromotionGoodsStock(goodsSku.getId(), quantity);
            }
        }
    }
}
