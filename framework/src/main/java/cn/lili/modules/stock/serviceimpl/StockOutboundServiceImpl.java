package cn.lili.modules.stock.serviceimpl;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSkuStockDTO;
import cn.lili.modules.goods.entity.enums.GoodsStockTypeEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.stock.entity.dos.StockOutbound;
import cn.lili.modules.stock.entity.dos.StockOutboundItem;
import cn.lili.modules.stock.entity.dto.StockFlowContext;
import cn.lili.modules.stock.entity.dto.StockOutboundAuditDTO;
import cn.lili.modules.stock.entity.dto.StockOutboundCreateDTO;
import cn.lili.modules.stock.entity.dto.StockOutboundItemDTO;
import cn.lili.modules.stock.entity.enums.StockFlowTypeEnum;
import cn.lili.modules.stock.entity.enums.StockOutboundStatusEnum;
import cn.lili.modules.stock.entity.params.StockOutboundSearchParams;
import cn.lili.modules.stock.entity.vos.StockOutboundVO;
import cn.lili.modules.stock.mapper.StockOutboundMapper;
import cn.lili.modules.stock.service.StockOutboundItemService;
import cn.lili.modules.stock.service.StockOutboundService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 商家出库单业务实现类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Service
public class StockOutboundServiceImpl extends ServiceImpl<StockOutboundMapper, StockOutbound> implements StockOutboundService {

    @Autowired
    private StockOutboundItemService stockOutboundItemService;

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    public IPage<StockOutbound> page(StockOutboundSearchParams params) {
        LambdaQueryWrapper<StockOutbound> queryWrapper = new LambdaQueryWrapper<>();
        if (CharSequenceUtil.isNotEmpty(params.getStoreId())) {
            queryWrapper.eq(StockOutbound::getStoreId, params.getStoreId());
        }
        if (CharSequenceUtil.isNotEmpty(params.getOutboundSn())) {
            queryWrapper.like(StockOutbound::getOutboundSn, params.getOutboundSn());
        }
        if (CharSequenceUtil.isNotEmpty(params.getStatus())) {
            queryWrapper.eq(StockOutbound::getStatus, params.getStatus());
        }
        if (CharSequenceUtil.isNotEmpty(params.getStartDate())) {
            Date start = DateUtil.beginOfDay(DateUtil.parse(params.getStartDate()));
            queryWrapper.ge(StockOutbound::getCreateTime, start);
        }
        if (CharSequenceUtil.isNotEmpty(params.getEndDate())) {
            Date end = DateUtil.endOfDay(DateUtil.parse(params.getEndDate()));
            queryWrapper.le(StockOutbound::getCreateTime, end);
        }
        queryWrapper.orderByDesc(StockOutbound::getCreateTime);
        return this.page(PageUtil.initPage(params), queryWrapper);
    }

    @Override
    public StockOutboundVO getDetail(String id) {
        StockOutbound outbound = OperationalJudgment.judgment(this.getById(id));
        if (outbound == null) {
            throw new ServiceException(ResultCode.STOCK_OUTBOUND_NOT_EXIST);
        }
        List<StockOutboundItem> items = stockOutboundItemService.listByOutboundId(id);
        StockOutboundVO vo = new StockOutboundVO();
        BeanUtil.copyProperties(outbound, vo);
        vo.setItems(items);
        return vo;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public StockOutboundVO createDraft(StockOutboundCreateDTO dto) {
        validateItems(dto.getItems());

        AuthUser user = UserContext.getCurrentUser();
        String storeId = user != null ? user.getStoreId() : null;
        String storeName = user != null ? user.getStoreName() : null;
        if (CharSequenceUtil.isEmpty(storeId)) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }

        int totalQuantity = dto.getItems().stream().mapToInt(StockOutboundItemDTO::getQuantity).sum();

        StockOutbound outbound = new StockOutbound();
        outbound.setOutboundSn(SnowFlake.createStr("SO"));
        outbound.setStoreId(storeId);
        outbound.setStoreName(storeName);
        outbound.setStatus(StockOutboundStatusEnum.DRAFT.name());
        outbound.setReasonId(dto.getReasonId());
        outbound.setReasonName(dto.getReasonName());
        outbound.setTotalQuantity(totalQuantity);
        outbound.setRemark(dto.getRemark());
        this.save(outbound);

        List<StockOutboundItem> items = buildItems(outbound.getId(), dto.getItems());
        stockOutboundItemService.saveBatch(items);

        StockOutboundVO vo = new StockOutboundVO();
        BeanUtil.copyProperties(outbound, vo);
        vo.setItems(items);
        return vo;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public StockOutboundVO updateDraft(String id, StockOutboundCreateDTO dto) {
        StockOutbound outbound = OperationalJudgment.judgment(this.getById(id));
        if (outbound == null) {
            throw new ServiceException(ResultCode.STOCK_OUTBOUND_NOT_EXIST);
        }
        if (!StockOutboundStatusEnum.DRAFT.name().equals(outbound.getStatus())
                && !StockOutboundStatusEnum.REJECTED.name().equals(outbound.getStatus())) {
            throw new ServiceException(ResultCode.STOCK_OUTBOUND_STATUS_ERROR);
        }

        validateItems(dto.getItems());
        int totalQuantity = dto.getItems().stream().mapToInt(StockOutboundItemDTO::getQuantity).sum();

        boolean updated = this.update(new LambdaUpdateWrapper<StockOutbound>()
                .set(StockOutbound::getReasonId, dto.getReasonId())
                .set(StockOutbound::getReasonName, dto.getReasonName())
                .set(StockOutbound::getRemark, dto.getRemark())
                .set(StockOutbound::getTotalQuantity, totalQuantity)
                .eq(StockOutbound::getId, id)
                .in(StockOutbound::getStatus, StockOutboundStatusEnum.DRAFT.name(), StockOutboundStatusEnum.REJECTED.name()));

        if (!updated) {
            throw new ServiceException(ResultCode.STOCK_OUTBOUND_STATUS_ERROR);
        }

        stockOutboundItemService.deleteByOutboundId(id);
        List<StockOutboundItem> items = buildItems(id, dto.getItems());
        stockOutboundItemService.saveBatch(items);

        return getDetail(id);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean submit(String id) {
        StockOutbound outbound = OperationalJudgment.judgment(this.getById(id));
        if (outbound == null) {
            throw new ServiceException(ResultCode.STOCK_OUTBOUND_NOT_EXIST);
        }
        if (!StockOutboundStatusEnum.DRAFT.name().equals(outbound.getStatus())
                && !StockOutboundStatusEnum.REJECTED.name().equals(outbound.getStatus())) {
            throw new ServiceException(ResultCode.STOCK_OUTBOUND_STATUS_ERROR);
        }

        List<StockOutboundItem> items = stockOutboundItemService.listByOutboundId(id);
        if (items == null || items.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }

        boolean updated = this.update(new LambdaUpdateWrapper<StockOutbound>()
                .set(StockOutbound::getStatus, StockOutboundStatusEnum.AUDITING.name())
                .set(StockOutbound::getAuditBy, null)
                .set(StockOutbound::getAuditTime, null)
                .set(StockOutbound::getAuditRemark, null)
                .eq(StockOutbound::getId, id)
                .in(StockOutbound::getStatus, StockOutboundStatusEnum.DRAFT.name(), StockOutboundStatusEnum.REJECTED.name()));

        if (!updated) {
            throw new ServiceException(ResultCode.STOCK_OUTBOUND_STATUS_ERROR);
        }
        return true;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean audit(String id, StockOutboundAuditDTO dto) {
        StockOutbound outbound = OperationalJudgment.judgment(this.getById(id));
        if (outbound == null) {
            throw new ServiceException(ResultCode.STOCK_OUTBOUND_NOT_EXIST);
        }
        if (!StockOutboundStatusEnum.AUDITING.name().equals(outbound.getStatus())) {
            throw new ServiceException(ResultCode.STOCK_OUTBOUND_STATUS_ERROR);
        }

        AuthUser user = UserContext.getCurrentUser();
        String operator = user != null ? user.getUsername() : "system";

        if (Boolean.TRUE.equals(dto.getPass())) {
            List<StockOutboundItem> items = stockOutboundItemService.listByOutboundId(id);
            if (items == null || items.isEmpty()) {
                throw new ServiceException(ResultCode.PARAMS_ERROR);
            }

            // 预校验库存充足性
            for (StockOutboundItem item : items) {
                Integer currentStock = goodsSkuService.getStock(item.getSkuId());
                if (currentStock == null || currentStock < item.getQuantity()) {
                    throw new ServiceException(ResultCode.GOODS_SKU_QUANTITY_NOT_ENOUGH,
                            "商品 [" + item.getGoodsName() + "] 可用库存不足，当前库存: " + (currentStock != null ? currentStock : 0));
                }
            }

            List<GoodsSkuStockDTO> stockDTOS = items.stream().map(item -> {
                GoodsSkuStockDTO stockDTO = new GoodsSkuStockDTO();
                stockDTO.setGoodsId(item.getGoodsId());
                stockDTO.setSkuId(item.getSkuId());
                stockDTO.setQuantity(item.getQuantity());
                stockDTO.setType(GoodsStockTypeEnum.SUB.name());
                return stockDTO;
            }).collect(Collectors.toList());

            // 原子扣减库存并发布流水事件
            goodsSkuService.updateStocksByType(stockDTOS, StockFlowContext.of(
                    StockFlowTypeEnum.STOCK_OUTBOUND,
                    outbound.getOutboundSn(),
                    operator,
                    "出库单审核通过扣减库存"
            ));

            boolean updated = this.update(new LambdaUpdateWrapper<StockOutbound>()
                    .set(StockOutbound::getStatus, StockOutboundStatusEnum.COMPLETED.name())
                    .set(StockOutbound::getAuditBy, operator)
                    .set(StockOutbound::getAuditTime, new Date())
                    .set(StockOutbound::getAuditRemark, dto.getAuditRemark())
                    .set(StockOutbound::getCompleteTime, new Date())
                    .eq(StockOutbound::getId, id)
                    .eq(StockOutbound::getStatus, StockOutboundStatusEnum.AUDITING.name()));

            if (!updated) {
                throw new ServiceException(ResultCode.STOCK_OUTBOUND_STATUS_ERROR);
            }
        } else {
            // 审核驳回
            boolean updated = this.update(new LambdaUpdateWrapper<StockOutbound>()
                    .set(StockOutbound::getStatus, StockOutboundStatusEnum.REJECTED.name())
                    .set(StockOutbound::getAuditBy, operator)
                    .set(StockOutbound::getAuditTime, new Date())
                    .set(StockOutbound::getAuditRemark, dto.getAuditRemark())
                    .eq(StockOutbound::getId, id)
                    .eq(StockOutbound::getStatus, StockOutboundStatusEnum.AUDITING.name()));

            if (!updated) {
                throw new ServiceException(ResultCode.STOCK_OUTBOUND_STATUS_ERROR);
            }
        }
        return true;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean cancel(String id) {
        StockOutbound outbound = OperationalJudgment.judgment(this.getById(id));
        if (outbound == null) {
            throw new ServiceException(ResultCode.STOCK_OUTBOUND_NOT_EXIST);
        }

        boolean updated = this.update(new LambdaUpdateWrapper<StockOutbound>()
                .set(StockOutbound::getStatus, StockOutboundStatusEnum.CANCELLED.name())
                .eq(StockOutbound::getId, id)
                .in(StockOutbound::getStatus,
                        StockOutboundStatusEnum.DRAFT.name(),
                        StockOutboundStatusEnum.AUDITING.name(),
                        StockOutboundStatusEnum.REJECTED.name()));

        if (!updated) {
            throw new ServiceException(ResultCode.STOCK_OUTBOUND_STATUS_ERROR);
        }
        return true;
    }

    /**
     * 校验明细数据：非空、数量大于0、禁止同单重复SKU、禁止电子卡券商品
     *
     * @param items 明细列表
     */
    private void validateItems(List<StockOutboundItemDTO> items) {
        if (items == null || items.isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "出库单明细不能为空");
        }
        Set<String> skuIdSet = new HashSet<>();
        for (StockOutboundItemDTO item : items) {
            if (CharSequenceUtil.isEmpty(item.getSkuId())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "明细商品SKU不能为空");
            }
            if (item.getQuantity() == null || item.getQuantity() <= 0) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "出库数量必须大于0");
            }
            if (!skuIdSet.add(item.getSkuId())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "同一张出库单中明细SKU不能重复");
            }

            GoodsSku sku = goodsSkuService.getGoodsSkuByIdFromCache(item.getSkuId());
            if (sku == null) {
                sku = goodsSkuService.getById(item.getSkuId());
            }
            if (sku == null) {
                throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
            }
            if (GoodsTypeEnum.E_COUPON.name().equals(sku.getGoodsType())) {
                throw new ServiceException(ResultCode.CARD_KEY_STOCK_MANUAL_FORBIDDEN);
            }
        }
    }

    /**
     * 将明细DTO转为明细实体
     *
     * @param outboundId 出库单ID
     * @param dtos       明细DTO列表
     * @return 明细实体列表
     */
    private List<StockOutboundItem> buildItems(String outboundId, List<StockOutboundItemDTO> dtos) {
        List<StockOutboundItem> items = new ArrayList<>();
        for (StockOutboundItemDTO dto : dtos) {
            GoodsSku sku = goodsSkuService.getGoodsSkuByIdFromCache(dto.getSkuId());
            if (sku == null) {
                sku = goodsSkuService.getById(dto.getSkuId());
            }

            StockOutboundItem item = new StockOutboundItem();
            item.setOutboundId(outboundId);
            item.setGoodsId(sku != null ? sku.getGoodsId() : dto.getGoodsId());
            item.setSkuId(dto.getSkuId());
            item.setGoodsName(sku != null ? sku.getGoodsName() : dto.getGoodsName());
            item.setSpecs(sku != null ? sku.getSpecs() : dto.getSpecs());
            item.setGoodsUnit(sku != null ? sku.getGoodsUnit() : dto.getGoodsUnit());
            item.setQuantity(dto.getQuantity());
            item.setItemRemark(dto.getItemRemark());
            items.add(item);
        }
        return items;
    }
}
