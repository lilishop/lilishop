/**
 * 作者：Mike
 * 日期：2026-07-31
 */
package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.convert.Convert;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.poi.excel.ExcelReader;
import cn.hutool.poi.excel.ExcelUtil;
import cn.hutool.poi.excel.ExcelWriter;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.goods.entity.dos.CardKey;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.CardKeyAddDTO;
import cn.lili.modules.goods.entity.dto.CardKeyImportDTO;
import cn.lili.modules.goods.entity.dto.search.CardKeySearchParams;
import cn.lili.modules.goods.entity.enums.CardKeyStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.vo.CardKeyImportResultVO;
import cn.lili.modules.goods.entity.vo.CardKeyPoolVO;
import cn.lili.modules.goods.entity.vo.CardKeyStatsVO;
import cn.lili.modules.goods.entity.vo.CardKeyVO;
import cn.lili.modules.goods.mapper.CardKeyMapper;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.goods.util.CardKeySecretUtil;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderPromotionTypeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 电子卡券卡密服务实现。
 * <p>
 * 卡密库内 AES 加密存储；商家授权接口返回明文。支付履约由 {@link #fulfillAfterPayment} 统一入口，
 * 勿与 VIRTUAL 核销链（TAKE）混用。
 *
 * @author Mike
 * @date 2026-07-31
 */
@Slf4j
@Service
public class CardKeyServiceImpl extends ServiceImpl<CardKeyMapper, CardKey> implements CardKeyService {

    private static final int IMPORT_MAX_ROWS = 10000;
    private static final int EXPORT_MAX_ROWS = 10000;
    private static final int MAX_BUY_QUANTITY = 999;
    private static final String FULFILL_FAIL_CANCEL_REASON = "库存不足，出库失败";

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    private GoodsService goodsService;

    @Autowired
    private StoreService storeService;

    @Autowired
    private CardKeySecretUtil cardKeySecretUtil;

    @Autowired
    private OrderMapper orderMapper;

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    private OrderService orderService;

    /**
     * {@inheritDoc}
     * <p>幂等：已 COMPLETED 直接跳过。失败路径触发系统 auto 退款，非买家售后。
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void fulfillAfterPayment(String orderSn) {
        Order order = orderMapper.selectOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, orderSn));
        if (order == null) {
            log.warn("电子卡券支付后履约跳过，订单不存在，orderSn={}", orderSn);
            return;
        }
        if (!OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            log.warn("电子卡券支付后履约跳过，非 E_COUPON 订单，orderSn={}，orderType={}", orderSn, order.getOrderType());
            return;
        }
        if (OrderStatusEnum.COMPLETED.name().equals(order.getOrderStatus())) {
            log.info("电子卡券支付后履约跳过，订单已完成，orderSn={}", orderSn);
            return;
        }
        log.info("电子卡券支付后履约开始，orderSn={}，orderStatus={}", orderSn, order.getOrderStatus());
        if (isPurchaseTypeOrder(order)) {
            try {
                allocateByOrder(orderSn);
                orderService.systemComplete(orderSn);
                log.info("电子卡券支付后履约完成，orderSn={}", orderSn);
            } catch (Exception e) {
                log.error("电子卡券支付后履约失败，orderSn={}", orderSn, e);
                voidOnSystemFailure(orderSn);
                orderItemService.getByOrderSn(orderSn)
                        .forEach(item -> syncSkuStock(item.getSkuId()));
                orderService.systemCancel(orderSn, FULFILL_FAIL_CANCEL_REASON, true);
            }
        } else {
            orderService.systemComplete(orderSn);
            log.info("电子卡券非购买型订单直接完成，orderSn={}", orderSn);
        }
    }

    /** {@inheritDoc} 前置校验：店铺权限、SKU 类型、审核/关店状态（S-06/S-07）。 */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public CardKeyImportResultVO importExcel(String skuId, MultipartFile file) {
        GoodsSku sku = validateSkuForManage(skuId, currentStoreId());
        List<CardKeyImportDTO> rows = parseImportFile(file);
        if (rows.size() > IMPORT_MAX_ROWS) {
            throw new ServiceException(ResultCode.CARD_KEY_IMPORT_LIMIT);
        }
        return importBatch(sku.getId(), rows);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public CardKeyPoolVO addCardKey(CardKeyAddDTO dto) {
        GoodsSku sku = validateSkuForManage(dto.getSkuId(), currentStoreId());
        CardKeyImportResultVO result = importBatch(sku.getId(), CollUtil.newArrayList(
                new CardKeyImportDTO(null, dto.getCardNo(), dto.getCardSecret())));
        if (result.getSuccessCount() == 0) {
            CardKeyImportResultVO.FailRow failRow = result.getFailRows().get(0);
            throw new ServiceException(ResultCode.ERROR, failRow.getReason());
        }
        CardKey cardKey = this.getOne(new LambdaQueryWrapper<CardKey>()
                .eq(CardKey::getSkuId, sku.getId())
                .eq(CardKey::getCardNo, dto.getCardNo().trim())
                .eq(CardKey::getDeleteFlag, false)
                .last("LIMIT 1"));
        return toPoolVO(cardKey);
    }

    @Override
    public IPage<CardKeyPoolVO> page(CardKeySearchParams params) {
        params.setStoreId(currentStoreId());
        if (CharSequenceUtil.isNotBlank(params.getSkuId())) {
            validateSkuForManage(params.getSkuId(), params.getStoreId());
        }
        validateTimeRange(params.getCreateTimeStart(), params.getCreateTimeEnd());
        LambdaQueryWrapper<CardKey> wrapper = buildListWrapper(params);
        Page<CardKey> page = this.page(Page.of(params.getPageNumber(), params.getPageSize()), wrapper);
        Page<CardKeyPoolVO> voPage = new Page<>(page.getCurrent(), page.getSize(), page.getTotal());
        voPage.setRecords(page.getRecords().stream().map(this::toPoolVO).collect(Collectors.toList()));
        return voPage;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void voidCard(String id) {
        CardKey cardKey = this.getById(id);
        if (cardKey == null || Boolean.TRUE.equals(cardKey.getDeleteFlag())) {
            throw new ServiceException(ResultCode.CARD_KEY_NOT_FOUND);
        }
        if (!currentStoreId().equals(cardKey.getStoreId())) {
            throw new ServiceException(ResultCode.CARD_KEY_STORE_ACCESS_DENIED);
        }
        validateSkuForManage(cardKey.getSkuId(), cardKey.getStoreId());
        if (!CardKeyStatusEnum.UNUSED.name().equals(cardKey.getStatus())) {
            throw new ServiceException(ResultCode.CARD_KEY_CANNOT_VOID);
        }
        this.update(new LambdaUpdateWrapper<CardKey>()
                .eq(CardKey::getId, id)
                .set(CardKey::getStatus, CardKeyStatusEnum.VOIDED.name()));
        syncSkuStock(cardKey.getSkuId());
    }

    @Override
    public CardKeyStatsVO stats(String skuId) {
        validateSkuForManage(skuId, currentStoreId());
        CardKeyStatsVO vo = new CardKeyStatsVO();
        vo.setSkuId(skuId);
        vo.setUnusedCount(countByStatus(skuId, CardKeyStatusEnum.UNUSED));
        vo.setAllocatedCount(countByStatus(skuId, CardKeyStatusEnum.ALLOCATED));
        vo.setVoidedCount(countByStatus(skuId, CardKeyStatusEnum.VOIDED));
        return vo;
    }

    @Override
    public void export(CardKeySearchParams params, HttpServletResponse response) {
        params.setStoreId(currentStoreId());
        if (CharSequenceUtil.isNotBlank(params.getSkuId())) {
            validateSkuForManage(params.getSkuId(), params.getStoreId());
        }
        validateTimeRange(params.getCreateTimeStart(), params.getCreateTimeEnd());
        LambdaQueryWrapper<CardKey> wrapper = buildListWrapper(params);
        long total = this.count(wrapper);
        if (total > EXPORT_MAX_ROWS) {
            throw new ServiceException(ResultCode.CARD_KEY_EXPORT_LIMIT);
        }
        List<CardKey> list = this.list(wrapper);
        String skuPart = CharSequenceUtil.blankToDefault(params.getSkuId(), "all");
        String filename = "card-key-" + skuPart + "-" + DateUtil.format(new Date(), "yyyyMMddHHmmss") + ".xlsx";
        ExcelWriter writer = ExcelUtil.getWriter(true);
        try {
            writer.writeHeadRow(CollUtil.newArrayList("卡号", "卡密", "状态", "导入时间", "发卡时间", "订单号"));
            for (CardKey cardKey : list) {
                writer.writeRow(CollUtil.newArrayList(
                        cardKey.getCardNo(),
                        cardKeySecretUtil.decrypt(cardKey.getCardSecret()),
                        cardKey.getStatus(),
                        cardKey.getCreateTime() == null ? "" : DateUtil.formatDateTime(cardKey.getCreateTime()),
                        cardKey.getAllocatedTime() == null ? "" : DateUtil.formatDateTime(cardKey.getAllocatedTime()),
                        CharSequenceUtil.blankToDefault(cardKey.getOrderSn(), "")));
            }
            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;charset=utf-8");
            response.setHeader("Content-Disposition", "attachment;filename=" + URLEncoder.encode(filename, StandardCharsets.UTF_8));
            ServletOutputStream out = response.getOutputStream();
            writer.flush(out, true);
        } catch (Exception e) {
            log.error("卡池导出失败", e);
            throw new ServiceException(ResultCode.ERROR);
        } finally {
            writer.close();
        }
    }

    @Override
    public void downloadImportTemplate(HttpServletResponse response) {
        ExcelWriter writer = ExcelUtil.getWriter(true);
        try {
            writer.writeHeadRow(CollUtil.newArrayList("卡号", "卡密"));
            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;charset=utf-8");
            response.setHeader("Content-Disposition",
                    "attachment;filename=" + URLEncoder.encode("card-key-import-template.xlsx", StandardCharsets.UTF_8));
            ServletOutputStream out = response.getOutputStream();
            writer.flush(out, true);
        } catch (Exception e) {
            log.error("卡密导入模板下载失败", e);
            throw new ServiceException(ResultCode.ERROR);
        } finally {
            writer.close();
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public CardKeyImportResultVO importBatch(String skuId, List<CardKeyImportDTO> rows) {
        GoodsSku sku = validateSkuForManage(skuId, currentStoreId());
        CardKeyImportResultVO result = new CardKeyImportResultVO();
        Set<String> batchCardNos = new HashSet<>();
        List<CardKey> toSave = new ArrayList<>();
        for (CardKeyImportDTO row : rows) {
            String cardNo = row.getCardNo() == null ? "" : row.getCardNo().trim();
            String cardSecret = row.getCardSecret() == null ? "" : row.getCardSecret().trim();
            if (CharSequenceUtil.isBlank(cardNo)) {
                addFailRow(result, row.getRow(), cardNo, ResultCode.CARD_KEY_CARD_NO_REQUIRED.message());
                continue;
            }
            if (CharSequenceUtil.isBlank(cardSecret)) {
                addFailRow(result, row.getRow(), cardNo, ResultCode.CARD_KEY_CARD_SECRET_REQUIRED.message());
                continue;
            }
            if (!batchCardNos.add(cardNo)) {
                addFailRow(result, row.getRow(), cardNo, ResultCode.CARD_KEY_DUPLICATE_CARD_NO.message());
                continue;
            }
            long exists = this.count(new LambdaQueryWrapper<CardKey>()
                    .eq(CardKey::getSkuId, sku.getId())
                    .eq(CardKey::getCardNo, cardNo)
                    .eq(CardKey::getDeleteFlag, false));
            if (exists > 0) {
                addFailRow(result, row.getRow(), cardNo, ResultCode.CARD_KEY_DUPLICATE_CARD_NO.message());
                continue;
            }
            CardKey cardKey = new CardKey();
            cardKey.setId(SnowFlake.getIdStr());
            cardKey.setSkuId(sku.getId());
            cardKey.setGoodsId(sku.getGoodsId());
            cardKey.setStoreId(sku.getStoreId());
            cardKey.setCardNo(cardNo);
            cardKey.setCardSecret(cardKeySecretUtil.encrypt(cardSecret));
            cardKey.setStatus(CardKeyStatusEnum.UNUSED.name());
            cardKey.setDeleteFlag(false);
            toSave.add(cardKey);
        }
        if (!toSave.isEmpty()) {
            this.saveBatch(toSave);
            syncSkuStock(sku.getId());
        }
        result.setSuccessCount(toSave.size());
        result.setFailCount(result.getFailRows().size());
        return result;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void syncSkuStock(String skuId) {
        GoodsSku goodsSku = goodsSkuService.getById(skuId);
        if (goodsSku == null) {
            return;
        }
        int poolStock = countUnusedPoolStock(skuId);
        goodsSkuService.updateStock(skuId, poolStock);
        // 商家商品列表读 li_goods.quantity，须同步 SPU 级库存
        goodsService.updateStock(goodsSku.getGoodsId());
    }

    @Override
    public int countUnusedPoolStock(String skuId) {
        return Math.toIntExact(this.count(new LambdaQueryWrapper<CardKey>()
                .eq(CardKey::getSkuId, skuId)
                .eq(CardKey::getStatus, CardKeyStatusEnum.UNUSED.name())
                .eq(CardKey::getDeleteFlag, false)));
    }

    /**
     * {@inheritDoc}
     * <p>任一步取卡不足则 voidOnSystemFailure 并抛异常，触发上层系统退款（EC-16）。
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void allocateByOrder(String orderSn) {
        Order order = orderMapper.selectOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, orderSn));
        if (order == null || !isPurchaseTypeOrder(order)) {
            return;
        }
        if (alreadyAllocated(orderSn)) {
            return;
        }
        List<OrderItem> orderItems = orderItemService.getByOrderSn(orderSn);
        List<CardKey> allocated = new ArrayList<>();
        try {
            for (OrderItem orderItem : orderItems) {
                GoodsSku sku = goodsSkuService.getById(orderItem.getSkuId());
                if (sku == null || !GoodsTypeEnum.E_COUPON.name().equals(sku.getGoodsType())) {
                    continue;
                }
                List<CardKey> cards = baseMapper.selectForAllocate(orderItem.getSkuId(), orderItem.getNum());
                if (cards.size() < orderItem.getNum()) {
                    throw new ServiceException(ResultCode.CARD_KEY_POOL_INSUFFICIENT);
                }
                Date now = new Date();
                for (CardKey card : cards) {
                    card.setStatus(CardKeyStatusEnum.ALLOCATED.name());
                    card.setOrderSn(orderSn);
                    card.setOrderItemSn(orderItem.getSn());
                    card.setMemberId(order.getMemberId());
                    card.setAllocatedTime(now);
                    this.updateById(card);
                    allocated.add(card);
                }
                syncSkuStock(orderItem.getSkuId());
            }
        } catch (RuntimeException ex) {
            voidOnSystemFailure(orderSn);
            throw ex;
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void voidOnSystemFailure(String orderSn) {
        List<CardKey> allocated = this.list(new LambdaQueryWrapper<CardKey>()
                .eq(CardKey::getOrderSn, orderSn)
                .eq(CardKey::getStatus, CardKeyStatusEnum.ALLOCATED.name())
                .eq(CardKey::getDeleteFlag, false));
        if (allocated.isEmpty()) {
            return;
        }
        Set<String> skuIds = new HashSet<>();
        for (CardKey cardKey : allocated) {
            this.update(new LambdaUpdateWrapper<CardKey>()
                    .eq(CardKey::getId, cardKey.getId())
                    .set(CardKey::getStatus, CardKeyStatusEnum.VOIDED.name())
                    .set(CardKey::getOrderSn, null)
                    .set(CardKey::getOrderItemSn, null)
                    .set(CardKey::getMemberId, null)
                    .set(CardKey::getAllocatedTime, null));
            skuIds.add(cardKey.getSkuId());
        }
        skuIds.forEach(this::syncSkuStock);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void softDeletePoolOnSkuRemove(String skuId) {
        this.update(new LambdaUpdateWrapper<CardKey>()
                .eq(CardKey::getSkuId, skuId)
                .in(CardKey::getStatus, CardKeyStatusEnum.UNUSED.name(), CardKeyStatusEnum.VOIDED.name())
                .eq(CardKey::getDeleteFlag, false)
                .set(CardKey::getDeleteFlag, true));
        syncSkuStock(skuId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void softDeletePoolOnStoreClose(String storeId) {
        List<GoodsSku> skus = goodsSkuService.list(new LambdaQueryWrapper<GoodsSku>()
                .eq(GoodsSku::getStoreId, storeId)
                .eq(GoodsSku::getGoodsType, GoodsTypeEnum.E_COUPON.name())
                .eq(GoodsSku::getDeleteFlag, false));
        for (GoodsSku sku : skus) {
            softDeletePoolOnSkuRemove(sku.getId());
        }
    }

    @Override
    public boolean isPurchaseTypeOrder(Order order) {
        return order != null
                && OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())
                && !OrderPromotionTypeEnum.GIFT.name().equals(order.getOrderPromotionType())
                && CharSequenceUtil.isBlank(order.getParentOrderSn());
    }

    @Override
    public boolean isECouponOnlyTrade(TradeDTO tradeDTO) {
        if (tradeDTO == null) {
            return false;
        }
        List<CartSkuVO> lines = tradeDTO.getCheckedSkuList();
        if (CollUtil.isEmpty(lines)) {
            lines = tradeDTO.getSkuList();
        }
        if (CollUtil.isEmpty(lines)) {
            return false;
        }
        return lines.stream().allMatch(line -> line.getGoodsSku() != null
                && GoodsTypeEnum.E_COUPON.name().equals(line.getGoodsSku().getGoodsType()));
    }

    @Override
    public void normalizeECouponTrade(TradeDTO tradeDTO) {
        if (!isECouponOnlyTrade(tradeDTO)) {
            return;
        }
        tradeDTO.setMemberAddress(null);
        tradeDTO.setStoreAddress(null);
        tradeDTO.setPlatformCoupon(null);
        tradeDTO.setStoreCoupons(new java.util.HashMap<>());
        tradeDTO.setSelectedGiftCardIds(new ArrayList<>());
        tradeDTO.setSkuPromotionDetail(new java.util.HashMap<>());
        if (tradeDTO.getCartList() != null) {
            for (CartVO cart : tradeDTO.getCartList()) {
                cart.setDeliveryMethod(DeliveryMethodEnum.VIRTUAL.name());
                cart.setFullDiscount(null);
                cart.setGiftPoint(null);
                if (cart.getSkuList() != null) {
                    for (CartSkuVO sku : cart.getSkuList()) {
                        sku.setDeliveryMethod(DeliveryMethodEnum.VIRTUAL.name());
                    }
                }
            }
        }
        if (tradeDTO.getSkuList() != null) {
            for (CartSkuVO sku : tradeDTO.getSkuList()) {
                sku.setDeliveryMethod(DeliveryMethodEnum.VIRTUAL.name());
            }
        }
        PriceDetailDTO priceDetail = tradeDTO.getPriceDetailDTO();
        if (priceDetail != null) {
            priceDetail.setFreightPrice(0D);
        }
    }

    @Override
    public void validateTradeForECoupon(TradeDTO tradeDTO) {
        List<CartSkuVO> checked = tradeDTO.getCheckedSkuList();
        if (CollUtil.isEmpty(checked)) {
            return;
        }
        long eCouponCount = checked.stream()
                .filter(s -> GoodsTypeEnum.E_COUPON.name().equals(s.getGoodsSku().getGoodsType()))
                .count();
        if (eCouponCount == 0) {
            return;
        }
        boolean hasNonECoupon = checked.stream()
                .anyMatch(s -> !GoodsTypeEnum.E_COUPON.name().equals(s.getGoodsSku().getGoodsType()));
        if (hasNonECoupon) {
            throw new ServiceException(ResultCode.CARD_KEY_MIXED_CHECKOUT);
        }
        if (!CartTypeEnum.BUY_NOW.equals(tradeDTO.getCartTypeEnum())) {
            throw new ServiceException(ResultCode.CARD_KEY_E_COUPON_CART_FORBIDDEN);
        }
        if (checked.size() > 1) {
            throw new ServiceException(ResultCode.CARD_KEY_MULTI_E_COUPON_SKU);
        }
        CartSkuVO eCouponSku = checked.get(0);
        validatePromotionForbidden(tradeDTO);
        validateStoreOpenForSell(eCouponSku.getStoreId());
        int num = eCouponSku.getNum() == null ? 0 : eCouponSku.getNum();
        int maxAllowed = Math.min(countUnusedPoolStock(eCouponSku.getGoodsSku().getId()), MAX_BUY_QUANTITY);
        if (num <= 0 || num > maxAllowed) {
            throw new ServiceException(ResultCode.CARD_KEY_POOL_INSUFFICIENT);
        }
        Double flowPrice = tradeDTO.getPriceDetailDTO() == null ? null : tradeDTO.getPriceDetailDTO().getFlowPrice();
        if (flowPrice == null || flowPrice <= 0) {
            throw new ServiceException(ResultCode.CARD_KEY_ZERO_PRICE);
        }
    }

    @Override
    public void validateAddToCart(String skuId, Integer num, String cartType) {
        GoodsSku sku = goodsSkuService.getById(skuId);
        if (sku == null || !GoodsTypeEnum.E_COUPON.name().equals(sku.getGoodsType())) {
            return;
        }
        if (CartTypeEnum.CART.name().equals(cartType)) {
            throw new ServiceException(ResultCode.CARD_KEY_E_COUPON_CART_FORBIDDEN);
        }
        if (num != null && num > Math.min(countUnusedPoolStock(skuId), MAX_BUY_QUANTITY)) {
            throw new ServiceException(ResultCode.CARD_KEY_POOL_INSUFFICIENT);
        }
        validateStoreOpenForSell(sku.getStoreId());
    }

    @Override
    public void validatePayOrder(Order order) {
        if (order == null || !OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            return;
        }
        validateStoreOpenForSell(order.getStoreId());
    }

    @Override
    public void enrichOrderDetail(OrderDetailVO orderDetailVO) {
        if (orderDetailVO == null || orderDetailVO.getOrder() == null || CollUtil.isEmpty(orderDetailVO.getOrderItems())) {
            return;
        }
        Order order = orderDetailVO.getOrder();
        if (!OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            return;
        }
        boolean delivered = OrderStatusEnum.COMPLETED.name().equals(order.getOrderStatus())
                && PayStatusEnum.PAID.name().equals(order.getPayStatus());
        for (OrderItem orderItem : orderDetailVO.getOrderItems()) {
            if (delivered) {
                List<CardKeyVO> cardKeys = listCardKeyVOByOrderItemSn(orderItem.getSn(), true);
                orderItem.setCardKeys(cardKeys);
                orderItem.setCardKeyDelivered(!cardKeys.isEmpty());
            } else {
                orderItem.setCardKeys(new ArrayList<>());
                orderItem.setCardKeyDelivered(false);
            }
        }
    }

    @Override
    public List<CardKeyVO> listCardKeyVOByOrderItemSn(String orderItemSn, boolean includeStatus) {
        List<CardKey> list = this.list(new LambdaQueryWrapper<CardKey>()
                .eq(CardKey::getOrderItemSn, orderItemSn)
                .eq(CardKey::getStatus, CardKeyStatusEnum.ALLOCATED.name())
                .eq(CardKey::getDeleteFlag, false)
                .orderByAsc(CardKey::getAllocatedTime));
        List<CardKeyVO> result = new ArrayList<>();
        for (CardKey cardKey : list) {
            CardKeyVO vo = new CardKeyVO();
            vo.setCardNo(cardKey.getCardNo());
            vo.setCardSecret(cardKeySecretUtil.decrypt(cardKey.getCardSecret()));
            vo.setAllocatedTime(cardKey.getAllocatedTime());
            if (includeStatus) {
                vo.setStatus(cardKey.getStatus());
            }
            result.add(vo);
        }
        return result;
    }

    /**
     * 卡池管理前置校验（API-S-01~S-07）：跨店、非 E_COUPON、关店、审核拒绝、SKU/SPU 已删。
     */
    private GoodsSku validateSkuForManage(String skuId, String storeId) {
        GoodsSku sku = goodsSkuService.getById(skuId);
        if (sku == null || Boolean.TRUE.equals(sku.getDeleteFlag())) {
            throw new ServiceException(ResultCode.CARD_KEY_SKU_DELETED);
        }
        if (!storeId.equals(sku.getStoreId())) {
            throw new ServiceException(ResultCode.CARD_KEY_STORE_ACCESS_DENIED);
        }
        if (!GoodsTypeEnum.E_COUPON.name().equals(sku.getGoodsType())) {
            throw new ServiceException(ResultCode.CARD_KEY_SKU_NOT_E_COUPON);
        }
        Store store = storeService.getById(storeId);
        if (store != null && StoreStatusEnum.CLOSED.name().equals(store.getStoreDisable())) {
            throw new ServiceException(ResultCode.CARD_KEY_STORE_CLOSED);
        }
        Goods goods = goodsService.getById(sku.getGoodsId());
        if (goods == null || Boolean.TRUE.equals(goods.getDeleteFlag())) {
            throw new ServiceException(ResultCode.CARD_KEY_SKU_DELETED);
        }
        if (GoodsAuthEnum.REFUSE.name().equals(goods.getAuthFlag())) {
            throw new ServiceException(ResultCode.CARD_KEY_GOODS_AUTH_REFUSE);
        }
        return sku;
    }

    private void validateStoreOpenForSell(String storeId) {
        Store store = storeService.getById(storeId);
        if (store != null && StoreStatusEnum.CLOSED.name().equals(store.getStoreDisable())) {
            throw new ServiceException(ResultCode.CARD_KEY_STORE_SELL_FORBIDDEN);
        }
    }

    /**
     * 拦截 E_COUPON 参与的一切促销：满减、优惠券、积分、拼团、礼品卡等（M-01 / EC-20）。
     */
    private void validatePromotionForbidden(TradeDTO tradeDTO) {
        CartTypeEnum cartType = tradeDTO.getCartTypeEnum();
        if (cartType != null && (CartTypeEnum.POINTS.equals(cartType)
                || CartTypeEnum.PINTUAN.equals(cartType)
                || CartTypeEnum.KANJIA.equals(cartType)
                || CartTypeEnum.VIRTUAL.equals(cartType))) {
            throw new ServiceException(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
        }
        if (tradeDTO.getPlatformCoupon() != null) {
            throw new ServiceException(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
        }
        if (tradeDTO.getStoreCoupons() != null && !tradeDTO.getStoreCoupons().isEmpty()) {
            throw new ServiceException(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
        }
        if (CollUtil.isNotEmpty(tradeDTO.getSelectedGiftCardIds())) {
            throw new ServiceException(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
        }
        if (tradeDTO.getSkuPromotionDetail() != null && !tradeDTO.getSkuPromotionDetail().isEmpty()) {
            throw new ServiceException(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
        }
        if (cartType != null && !CartTypeEnum.BUY_NOW.equals(cartType) && !CartTypeEnum.CART.equals(cartType)) {
            throw new ServiceException(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
        }
        PriceDetailDTO priceDetail = tradeDTO.getPriceDetailDTO();
        if (priceDetail != null) {
            if (priceDetail.getPayPoint() != null && priceDetail.getPayPoint() > 0) {
                throw new ServiceException(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
            }
            if (priceDetail.getDiscountPrice() != null && priceDetail.getDiscountPrice() > 0) {
                throw new ServiceException(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
            }
            if (priceDetail.getCouponPrice() != null && priceDetail.getCouponPrice() > 0) {
                throw new ServiceException(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
            }
            if (priceDetail.getGiftCardPrice() != null && priceDetail.getGiftCardPrice() > 0) {
                throw new ServiceException(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
            }
            if (CollUtil.isNotEmpty(priceDetail.getJoinPromotion())) {
                throw new ServiceException(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
            }
        }
        if (CollUtil.isNotEmpty(tradeDTO.getCartList())) {
            for (CartVO cart : tradeDTO.getCartList()) {
                if (cart.getFullDiscount() != null) {
                    throw new ServiceException(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
                }
                if (cart.getGiftPoint() != null && cart.getGiftPoint() > 0) {
                    throw new ServiceException(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
                }
            }
        }
        for (CartSkuVO cartSkuVO : tradeDTO.getCheckedSkuList()) {
            if (cartSkuVO.getPromotionMap() != null && !cartSkuVO.getPromotionMap().isEmpty()) {
                throw new ServiceException(ResultCode.CARD_KEY_PROMOTION_FORBIDDEN);
            }
        }
    }

    private LambdaQueryWrapper<CardKey> buildListWrapper(CardKeySearchParams params) {
        LambdaQueryWrapper<CardKey> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(CardKey::getStoreId, params.getStoreId());
        wrapper.eq(CharSequenceUtil.isNotBlank(params.getSkuId()), CardKey::getSkuId, params.getSkuId());
        wrapper.eq(CharSequenceUtil.isNotBlank(params.getGoodsId()), CardKey::getGoodsId, params.getGoodsId());
        wrapper.eq(CharSequenceUtil.isNotBlank(params.getStatus()), CardKey::getStatus, params.getStatus());
        wrapper.like(CharSequenceUtil.isNotBlank(params.getCardNo()), CardKey::getCardNo, params.getCardNo());
        if (CharSequenceUtil.isNotBlank(params.getCreateTimeStart())) {
            wrapper.ge(CardKey::getCreateTime, DateUtil.parse(params.getCreateTimeStart()));
        }
        if (CharSequenceUtil.isNotBlank(params.getCreateTimeEnd())) {
            wrapper.le(CardKey::getCreateTime, DateUtil.parse(params.getCreateTimeEnd()));
        }
        wrapper.orderByDesc(CardKey::getCreateTime);
        return wrapper;
    }

    private void validateTimeRange(String start, String end) {
        if (CharSequenceUtil.isNotBlank(start) && CharSequenceUtil.isNotBlank(end)) {
            if (DateUtil.parse(start).after(DateUtil.parse(end))) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "导入时间区间无效");
            }
        }
    }

    private int countByStatus(String skuId, CardKeyStatusEnum status) {
        return Math.toIntExact(this.count(new LambdaQueryWrapper<CardKey>()
                .eq(CardKey::getSkuId, skuId)
                .eq(CardKey::getStatus, status.name())
                .eq(CardKey::getDeleteFlag, false)));
    }

    private CardKeyPoolVO toPoolVO(CardKey cardKey) {
        CardKeyPoolVO vo = new CardKeyPoolVO();
        vo.setId(cardKey.getId());
        vo.setSkuId(cardKey.getSkuId());
        vo.setGoodsId(cardKey.getGoodsId());
        vo.setCardNo(cardKey.getCardNo());
        vo.setCardSecret(cardKeySecretUtil.decrypt(cardKey.getCardSecret()));
        vo.setStatus(cardKey.getStatus());
        vo.setOrderSn(cardKey.getOrderSn());
        vo.setOrderItemSn(cardKey.getOrderItemSn());
        vo.setAllocatedTime(cardKey.getAllocatedTime());
        vo.setCreateTime(cardKey.getCreateTime());
        return vo;
    }

    private List<CardKeyImportDTO> parseImportFile(MultipartFile file) {
        try (ExcelReader reader = ExcelUtil.getReader(file.getInputStream())) {
            // 第 1 行表头，从第 2 行起读数据（Hutool 行号从 0 起，故 startRowIndex = 1）
            List<List<Object>> raw = reader.read(1, reader.getRowCount() - 1);
            List<CardKeyImportDTO> rows = new ArrayList<>();
            int rowNum = 2;
            for (List<Object> line : raw) {
                String cardNo = line.size() > 0 ? Convert.toStr(line.get(0)) : "";
                String cardSecret = line.size() > 1 ? Convert.toStr(line.get(1)) : "";
                rows.add(new CardKeyImportDTO(rowNum++, cardNo, cardSecret));
            }
            return rows;
        } catch (Exception e) {
            log.error("解析卡密导入文件失败", e);
            throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR);
        }
    }

    private boolean alreadyAllocated(String orderSn) {
        return this.count(new LambdaQueryWrapper<CardKey>()
                .eq(CardKey::getOrderSn, orderSn)
                .eq(CardKey::getStatus, CardKeyStatusEnum.ALLOCATED.name())
                .eq(CardKey::getDeleteFlag, false)) > 0;
    }

    private void addFailRow(CardKeyImportResultVO result, Integer row, String cardNo, String reason) {
        CardKeyImportResultVO.FailRow failRow = new CardKeyImportResultVO.FailRow();
        failRow.setRow(row);
        failRow.setCardNo(cardNo);
        failRow.setReason(reason);
        result.getFailRows().add(failRow);
    }

    private String currentStoreId() {
        AuthUser user = UserContext.getCurrentUser();
        if (user == null || CharSequenceUtil.isBlank(user.getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        return user.getStoreId();
    }
}
