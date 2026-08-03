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
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.enums.CardKeyFulfillStatusEnum;
import cn.lili.modules.goods.entity.enums.CardKeyStatusEnum;
import cn.lili.modules.order.order.entity.vo.GiftECouponOrderSummaryVO;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.vo.CardKeyImportResultVO;
import cn.lili.modules.goods.entity.vo.CardKeyPoolVO;
import cn.lili.modules.goods.entity.vo.CardKeyStatsVO;
import cn.lili.modules.goods.entity.vo.CardKeyVO;
import cn.lili.modules.goods.mapper.CardKeyMapper;
import cn.lili.modules.goods.service.CardKeyService;
import cn.lili.modules.goods.service.GiftFulfillmentAlertPublisher;
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
 * 卡密库内明文存储。支付履约由 {@link #fulfillAfterPayment} 统一入口，
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
    private static final String GIFT_FULFILL_FAIL_CANCEL_REASON = "满赠电子卡券发放失败";

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

    @Autowired
    private GiftFulfillmentAlertPublisher giftFulfillmentAlertPublisher;

    @Autowired
    private OrderLogService orderLogService;

    /**
     * {@inheritDoc}
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void fulfillAfterPayment(String orderSn) {
        Order order = orderMapper.selectForUpdateBySn(orderSn);
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
        if (!isECouponFulfillmentEligible(order)) {
            log.warn("电子卡券支付后履约拒绝，状态不满足门禁，orderSn={}，payStatus={}，orderStatus={}",
                    orderSn, order.getPayStatus(), order.getOrderStatus());
            return;
        }
        log.info("电子卡券支付后履约开始，orderSn={}，orderStatus={}，promotionType={}",
                orderSn, order.getOrderStatus(), order.getOrderPromotionType());
        if (isGiftECouponOrder(order)) {
            fulfillGiftECouponOrder(order);
        } else if (isPintuanECouponOrder(order)) {
            log.info("电子卡券拼团订单等待成团，orderSn={}", orderSn);
        } else if (isPurchaseTypeOrder(order)) {
            fulfillPurchaseECouponOrder(order);
        } else {
            log.error("电子卡券支付后履约拒绝，未知促销类型，orderSn={}，promotionType={}",
                    orderSn, order.getOrderPromotionType());
        }
    }

    /**
     * 购买型 E_COUPON：分配卡密后通过 {@link OrderService#systemCompleteECoupon} 完成订单；失败则作废并取消。
     */
    private void fulfillPurchaseECouponOrder(Order order) {
        String orderSn = order.getSn();
        try {
            allocateByOrder(orderSn);
            if (!orderService.systemCompleteECoupon(orderSn)) {
                log.warn("电子卡券支付后履约完成 CAS 未生效，orderSn={}", orderSn);
                return;
            }
            log.info("电子卡券支付后履约完成，orderSn={}", orderSn);
        } catch (Exception e) {
            log.error("电子卡券支付后履约失败，orderSn={}", orderSn, e);
            voidOnSystemFailure(orderSn);
            orderItemService.getByOrderSn(orderSn)
                    .forEach(item -> syncSkuStock(item.getSkuId()));
            orderService.systemCancelECoupon(orderSn, FULFILL_FAIL_CANCEL_REASON, true);
        }
    }

    /**
     * 满赠 E_COUPON 子单：分配卡密后完成子单；失败则作废、取消子单并告警主单。
     */
    private void fulfillGiftECouponOrder(Order giftOrder) {
        String orderSn = giftOrder.getSn();
        Order parentOrder = CharSequenceUtil.isBlank(giftOrder.getParentOrderSn()) ? null
                : orderMapper.selectOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, giftOrder.getParentOrderSn()));
        try {
            allocateGiftByOrder(orderSn);
            if (!orderService.systemCompleteECoupon(orderSn)) {
                log.warn("满赠电子卡券子单履约完成 CAS 未生效，orderSn={}", orderSn);
                return;
            }
            log.info("满赠电子卡券子单履约完成，orderSn={}", orderSn);
        } catch (Exception e) {
            log.error("满赠电子卡券子单履约失败，orderSn={}", orderSn, e);
            voidOnSystemFailure(orderSn);
            orderItemService.getByOrderSn(orderSn)
                    .forEach(item -> syncSkuStock(item.getSkuId()));
            orderService.systemCancelECoupon(orderSn, GIFT_FULFILL_FAIL_CANCEL_REASON, false);
            giftFulfillmentAlertPublisher.onGiftFulfillmentFailure(giftOrder, parentOrder, e.getMessage());
            writeGiftFulfillmentFailureLogs(giftOrder, parentOrder, e.getMessage());
        }
    }

    private void writeGiftFulfillmentFailureLogs(Order giftOrder, Order parentOrder, String reason) {
        String detail = CharSequenceUtil.blankToDefault(reason, "卡池库存不足");
        OrderLog giftLog = new OrderLog(
                giftOrder.getSn(),
                "-1",
                UserEnums.SYSTEM.name(),
                UserEnums.SYSTEM.getRole(),
                "满赠电子卡券发卡失败，赠品子单已取消。原因：" + detail);
        orderLogService.save(giftLog);
        if (parentOrder != null) {
            OrderLog parentLog = new OrderLog(
                    parentOrder.getSn(),
                    "-1",
                    UserEnums.SYSTEM.name(),
                    UserEnums.SYSTEM.getRole(),
                    "满赠赠品（子单 " + giftOrder.getSn() + "）卡密发放失败，主订单不受影响");
            orderLogService.save(parentLog);
        }
    }

    /** {@inheritDoc} 前置校验：店铺权限、SKU 类型、审核/关店状态。 */
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
        vo.setReservedCount(countByStatus(skuId, CardKeyStatusEnum.RESERVED));
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
        int availableStock = countUnusedPoolStock(skuId);
        goodsSkuService.updateStockFromCardKeySync(skuId, availableStock);
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
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void allocateByOrder(String orderSn) {
        Order order = orderMapper.selectForUpdateBySn(orderSn);
        if (order == null || !isPurchaseTypeOrder(order)) {
            return;
        }
        if (alreadyBound(orderSn, CardKeyStatusEnum.ALLOCATED)) {
            return;
        }
        bindCardsForOrder(order, orderSn, CardKeyStatusEnum.ALLOCATED);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void reserveByOrder(String orderSn) {
        Order order = orderMapper.selectForUpdateBySn(orderSn);
        if (order == null || !isPintuanECouponOrder(order)) {
            return;
        }
        if (!isECouponFulfillmentEligible(order)) {
            log.warn("电子卡券拼团预占拒绝，状态不满足门禁，orderSn={}，payStatus={}，orderStatus={}",
                    orderSn, order.getPayStatus(), order.getOrderStatus());
            return;
        }
        if (alreadyBound(orderSn, CardKeyStatusEnum.RESERVED)
                || alreadyBound(orderSn, CardKeyStatusEnum.ALLOCATED)) {
            return;
        }
        bindCardsForOrder(order, orderSn, CardKeyStatusEnum.RESERVED);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void confirmReservationAndComplete(String orderSn) {
        Order order = orderMapper.selectForUpdateBySn(orderSn);
        if (order == null || !isPintuanECouponOrder(order)) {
            return;
        }
        List<CardKey> reserved = baseMapper.selectReservedByOrder(orderSn);
        if (reserved.isEmpty()) {
            if (alreadyBound(orderSn, CardKeyStatusEnum.ALLOCATED)) {
                orderService.systemCompleteECoupon(orderSn);
            }
            return;
        }
        Date now = new Date();
        Set<String> skuIds = new HashSet<>();
        for (CardKey card : reserved) {
            card.setStatus(CardKeyStatusEnum.ALLOCATED.name());
            card.setAllocatedTime(now);
            this.updateById(card);
            skuIds.add(card.getSkuId());
        }
        skuIds.forEach(this::syncSkuStock);
        orderService.systemCompleteECoupon(orderSn);
        log.info("拼团电子卡券预占确认完成，orderSn={}", orderSn);
    }

    /**
     * {@inheritDoc}
     * <p>
     * 先对订单行加锁，再释放 RESERVED，避免与成团/取消并发冲突。
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void releaseReservation(String orderSn) {
        orderMapper.selectForUpdateBySn(orderSn);
        List<CardKey> reserved = baseMapper.selectReservedByOrder(orderSn);
        if (reserved.isEmpty()) {
            return;
        }
        Set<String> skuIds = new HashSet<>();
        for (CardKey cardKey : reserved) {
            this.update(new LambdaUpdateWrapper<CardKey>()
                    .eq(CardKey::getId, cardKey.getId())
                    .set(CardKey::getStatus, CardKeyStatusEnum.UNUSED.name())
                    .set(CardKey::getOrderSn, null)
                    .set(CardKey::getOrderItemSn, null)
                    .set(CardKey::getMemberId, null)
                    .set(CardKey::getAllocatedTime, null));
            skuIds.add(cardKey.getSkuId());
        }
        skuIds.forEach(this::syncSkuStock);
        log.info("拼团电子卡券预占已释放，orderSn={}", orderSn);
    }

    /**
     * 满赠子单分配卡密（调用方已持有订单行锁）。
     */
    private void allocateGiftByOrder(String orderSn) {
        Order order = orderMapper.selectForUpdateBySn(orderSn);
        if (order == null || !isGiftECouponOrder(order)) {
            return;
        }
        if (alreadyBound(orderSn, CardKeyStatusEnum.ALLOCATED)) {
            return;
        }
        bindCardsForOrder(order, orderSn, CardKeyStatusEnum.ALLOCATED);
    }

    /**
     * 按订单明细从卡池绑定卡密；购买型 ALLOCATED 失败时触发 {@link #voidOnSystemFailure}。
     *
     * @param order        已加锁的订单（可为 null）
     * @param orderSn      订单号
     * @param targetStatus ALLOCATED 或 RESERVED
     */
    private void bindCardsForOrder(Order order, String orderSn, CardKeyStatusEnum targetStatus) {
        if (order == null) {
            return;
        }
        List<OrderItem> orderItems = orderItemService.getByOrderSn(orderSn);
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
                    card.setStatus(targetStatus.name());
                    card.setOrderSn(orderSn);
                    card.setOrderItemSn(orderItem.getSn());
                    card.setMemberId(order.getMemberId());
                    if (CardKeyStatusEnum.ALLOCATED.equals(targetStatus)) {
                        card.setAllocatedTime(now);
                    }
                    this.updateById(card);
                }
                syncSkuStock(orderItem.getSkuId());
            }
        } catch (RuntimeException ex) {
            if (CardKeyStatusEnum.ALLOCATED.equals(targetStatus) && isPurchaseTypeOrder(order)) {
                voidOnSystemFailure(orderSn);
            }
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

    /**
     * {@inheritDoc}
     */
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
    public boolean isGiftECouponOrder(Order order) {
        return order != null
                && OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())
                && OrderPromotionTypeEnum.GIFT.name().equals(order.getOrderPromotionType());
    }

    @Override
    public boolean isPintuanECouponOrder(Order order) {
        return order != null
                && OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())
                && OrderPromotionTypeEnum.PINTUAN.name().equals(order.getOrderPromotionType());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isPurchaseTypeOrder(Order order) {
        if (order == null || !OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            return false;
        }
        if (CharSequenceUtil.isNotBlank(order.getParentOrderSn())) {
            return false;
        }
        String promotionType = order.getOrderPromotionType();
        return OrderPromotionTypeEnum.NORMAL.name().equals(promotionType)
                || OrderPromotionTypeEnum.POINTS.name().equals(promotionType)
                || OrderPromotionTypeEnum.KANJIA.name().equals(promotionType);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isECouponFulfillmentEligible(Order order) {
        if (order == null || !OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())) {
            return false;
        }
        if (!PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
            return false;
        }
        return OrderStatusEnum.PAID.name().equals(order.getOrderStatus());
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
        if (tradeDTO.getCartList() != null) {
            for (CartVO cart : tradeDTO.getCartList()) {
                cart.setDeliveryMethod(DeliveryMethodEnum.VIRTUAL.name());
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
        if (CartTypeEnum.CART.equals(tradeDTO.getCartTypeEnum())) {
            throw new ServiceException(ResultCode.CARD_KEY_E_COUPON_CART_FORBIDDEN);
        }
        if (checked.size() > 1) {
            throw new ServiceException(ResultCode.CARD_KEY_MULTI_E_COUPON_SKU);
        }
        CartSkuVO eCouponSku = checked.get(0);
        validateStoreOpenForSell(eCouponSku.getStoreId());
        int num = eCouponSku.getNum() == null ? 0 : eCouponSku.getNum();
        int maxAllowed = Math.min(countUnusedPoolStock(eCouponSku.getGoodsSku().getId()), MAX_BUY_QUANTITY);
        if (num <= 0 || num > maxAllowed) {
            throw new ServiceException(ResultCode.CARD_KEY_POOL_INSUFFICIENT);
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
        if (orderDetailVO == null || orderDetailVO.getOrder() == null) {
            return;
        }
        Order order = orderDetailVO.getOrder();
        if (OrderTypeEnum.E_COUPON.name().equals(order.getOrderType())
                && CollUtil.isNotEmpty(orderDetailVO.getOrderItems())) {
            for (OrderItem orderItem : orderDetailVO.getOrderItems()) {
                applyCardKeyFulfillToOrderItem(order, orderItem);
            }
        }
        if (!isGiftECouponOrder(order) && CharSequenceUtil.isBlank(order.getParentOrderSn())) {
            orderDetailVO.setGiftECouponOrders(loadGiftECouponSummaries(order.getSn()));
        }
    }

    private void applyCardKeyFulfillToOrderItem(Order order, OrderItem orderItem) {
        CardKeyFulfillStatusEnum status = resolveCardKeyFulfillStatus(order);
        String message = resolveCardKeyFulfillMessage(order, status);
        List<CardKeyVO> cardKeys = CardKeyFulfillStatusEnum.DELIVERED.equals(status)
                ? listCardKeyVOByOrderItemSn(orderItem.getSn(), true)
                : new ArrayList<>();
        if (CardKeyFulfillStatusEnum.DELIVERED.equals(status) && cardKeys.isEmpty()) {
            status = CardKeyFulfillStatusEnum.FAILED;
            message = CharSequenceUtil.blankToDefault(order.getCancelReason(), "订单已完成但未找到已分配卡密");
        }
        orderItem.setCardKeyFulfillStatus(status.name());
        orderItem.setCardKeyFulfillMessage(message);
        orderItem.setCardKeys(cardKeys);
        orderItem.setCardKeyDelivered(CardKeyFulfillStatusEnum.DELIVERED.equals(status) && !cardKeys.isEmpty());
    }

    private CardKeyFulfillStatusEnum resolveCardKeyFulfillStatus(Order order) {
        if (OrderStatusEnum.CANCELLED.name().equals(order.getOrderStatus())) {
            return CardKeyFulfillStatusEnum.FAILED;
        }
        if (OrderStatusEnum.COMPLETED.name().equals(order.getOrderStatus())
                && PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
            return CardKeyFulfillStatusEnum.DELIVERED;
        }
        if (isPintuanECouponOrder(order)) {
            return CardKeyFulfillStatusEnum.PENDING;
        }
        if (PayStatusEnum.PAID.name().equals(order.getPayStatus())
                || OrderStatusEnum.PAID.name().equals(order.getOrderStatus())) {
            return CardKeyFulfillStatusEnum.PENDING;
        }
        return CardKeyFulfillStatusEnum.PENDING;
    }

    private String resolveCardKeyFulfillMessage(Order order, CardKeyFulfillStatusEnum status) {
        if (CardKeyFulfillStatusEnum.FAILED.equals(status)) {
            if (GIFT_FULFILL_FAIL_CANCEL_REASON.equals(order.getCancelReason())) {
                return isGiftECouponOrder(order)
                        ? "满赠电子卡券发放失败，赠品子单已取消，主订单不受影响"
                        : order.getCancelReason();
            }
            return CharSequenceUtil.blankToDefault(order.getCancelReason(), "卡密发放失败");
        }
        if (CardKeyFulfillStatusEnum.DELIVERED.equals(status)) {
            return null;
        }
        if (isPintuanECouponOrder(order)) {
            return "拼团成功后发放卡密";
        }
        if (isGiftECouponOrder(order)) {
            return "满赠卡密发放中，请稍后刷新";
        }
        return "卡密发放中，请稍后刷新";
    }

    private List<GiftECouponOrderSummaryVO> loadGiftECouponSummaries(String parentOrderSn) {
        List<Order> giftOrders = orderMapper.selectList(new LambdaQueryWrapper<Order>()
                .eq(Order::getParentOrderSn, parentOrderSn)
                .eq(Order::getOrderPromotionType, OrderPromotionTypeEnum.GIFT.name())
                .eq(Order::getOrderType, OrderTypeEnum.E_COUPON.name()));
        if (CollUtil.isEmpty(giftOrders)) {
            return new ArrayList<>();
        }
        List<GiftECouponOrderSummaryVO> summaries = new ArrayList<>();
        for (Order giftOrder : giftOrders) {
            GiftECouponOrderSummaryVO summary = new GiftECouponOrderSummaryVO();
            summary.setOrderSn(giftOrder.getSn());
            summary.setParentOrderSn(giftOrder.getParentOrderSn());
            summary.setOrderStatus(giftOrder.getOrderStatus());
            CardKeyFulfillStatusEnum status = resolveCardKeyFulfillStatus(giftOrder);
            summary.setCardKeyFulfillStatus(status.name());
            summary.setCardKeyFulfillMessage(resolveCardKeyFulfillMessage(giftOrder, status));
            List<OrderItem> giftItems = orderItemService.getByOrderSn(giftOrder.getSn());
            if (CollUtil.isNotEmpty(giftItems)) {
                OrderItem first = giftItems.get(0);
                summary.setGoodsName(first.getGoodsName());
                summary.setSkuId(first.getSkuId());
            }
            List<CardKeyVO> cardKeys = new ArrayList<>();
            if (CardKeyFulfillStatusEnum.DELIVERED.equals(status)) {
                for (OrderItem giftItem : giftItems) {
                    cardKeys.addAll(listCardKeyVOByOrderItemSn(giftItem.getSn(), true));
                }
                if (cardKeys.isEmpty()) {
                    summary.setCardKeyFulfillStatus(CardKeyFulfillStatusEnum.FAILED.name());
                    summary.setCardKeyFulfillMessage(
                            CharSequenceUtil.blankToDefault(giftOrder.getCancelReason(), "订单已完成但未找到已分配卡密"));
                }
            }
            summary.setCardKeys(cardKeys);
            summary.setCardKeyDelivered(!cardKeys.isEmpty());
            summaries.add(summary);
        }
        return summaries;
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
     * 卡池管理前置校验：跨店、非 E_COUPON、关店、审核拒绝、SKU/SPU 已删。
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

    private boolean alreadyBound(String orderSn, CardKeyStatusEnum status) {
        return this.count(new LambdaQueryWrapper<CardKey>()
                .eq(CardKey::getOrderSn, orderSn)
                .eq(CardKey::getStatus, status.name())
                .eq(CardKey::getDeleteFlag, false)) > 0;
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
        return alreadyBound(orderSn, CardKeyStatusEnum.ALLOCATED);
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
