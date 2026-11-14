package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.support.CardKeyPoolSupport;
import cn.lili.modules.goods.support.CardKeyAllocationSupport;
import cn.lili.modules.order.order.serviceimpl.ECouponFulfillmentService;
import cn.lili.modules.goods.support.CardKeyImportSupport;
import cn.lili.modules.goods.support.CardKeyTradeSupport;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
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
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 电子卡券卡密服务实现。
 * <p>
 * 卡密库内明文存储。{@link #fulfillAfterPayment} 保留支付履约兼容入口，
 * 普通购买、拼团与满赠均委托独立履约编排，不反向调用完整订单服务；勿与 VIRTUAL 核销链（TAKE）混用。
 *
 * @author Mike
 * @date 2026-07-31
 */
@Slf4j
@Service
public class CardKeyServiceImpl extends ServiceImpl<CardKeyMapper, CardKey> implements CardKeyService {

    @Autowired
    private CardKeyAllocationSupport cardKeyAllocationSupport;

    @Autowired
    private ECouponFulfillmentService eCouponFulfillmentService;

    @Autowired
    private CardKeyPoolSupport cardKeyPoolSupport;

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

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void fulfillAfterPayment(String orderSn) {
        eCouponFulfillmentService.fulfillAfterPayment(orderSn);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public CardKeyImportResultVO importExcel(String skuId, MultipartFile file) {
        GoodsSku sku = validateSkuForManage(skuId, currentStoreId());
        List<CardKeyImportDTO> rows = CardKeyImportSupport.parseImportFile(file);
        if (rows.size() > CardKeyImportSupport.IMPORT_MAX_ROWS) {
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
        CardKeyImportSupport.validateTimeRange(params.getCreateTimeStart(), params.getCreateTimeEnd());
        LambdaQueryWrapper<CardKey> wrapper = CardKeyImportSupport.buildListWrapper(params);
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
        CardKeyImportSupport.validateTimeRange(params.getCreateTimeStart(), params.getCreateTimeEnd());
        LambdaQueryWrapper<CardKey> wrapper = CardKeyImportSupport.buildListWrapper(params);
        long total = this.count(wrapper);
        if (total > CardKeyImportSupport.EXPORT_MAX_ROWS) {
            throw new ServiceException(ResultCode.CARD_KEY_EXPORT_LIMIT);
        }
        List<CardKey> list = this.list(wrapper);
        CardKeyImportSupport.export(params, list, cardKeySecretUtil, response);
    }

    @Override
    public void downloadImportTemplate(HttpServletResponse response) {
        CardKeyImportSupport.downloadImportTemplate(response);
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
                CardKeyImportSupport.addFailRow(result, row.getRow(), cardNo, ResultCode.CARD_KEY_CARD_NO_REQUIRED.message());
                continue;
            }
            if (CharSequenceUtil.isBlank(cardSecret)) {
                CardKeyImportSupport.addFailRow(result, row.getRow(), cardNo, ResultCode.CARD_KEY_CARD_SECRET_REQUIRED.message());
                continue;
            }
            if (!batchCardNos.add(cardNo)) {
                CardKeyImportSupport.addFailRow(result, row.getRow(), cardNo, ResultCode.CARD_KEY_DUPLICATE_CARD_NO.message());
                continue;
            }
            long exists = this.count(new LambdaQueryWrapper<CardKey>()
                    .eq(CardKey::getSkuId, sku.getId())
                    .eq(CardKey::getCardNo, cardNo)
                    .eq(CardKey::getDeleteFlag, false));
            if (exists > 0) {
                CardKeyImportSupport.addFailRow(result, row.getRow(), cardNo, ResultCode.CARD_KEY_DUPLICATE_CARD_NO.message());
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
        cardKeyPoolSupport.syncSkuStock(skuId);
    }

    @Override
    public int countUnusedPoolStock(String skuId) {
        return cardKeyPoolSupport.countUnusedPoolStock(skuId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void allocateByOrder(String orderSn) {
        cardKeyAllocationSupport.allocateByOrder(orderSn);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void reserveByOrder(String orderSn) {
        eCouponFulfillmentService.reserveByOrder(orderSn);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void confirmReservationAndComplete(String orderSn) {
        eCouponFulfillmentService.confirmReservationAndComplete(orderSn);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void releaseReservation(String orderSn) {
        eCouponFulfillmentService.releaseReservation(orderSn);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void voidOnSystemFailure(String orderSn) {
        cardKeyAllocationSupport.voidOnSystemFailure(orderSn);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void softDeletePoolOnSkuRemove(String skuId) {
        cardKeyPoolSupport.softDeletePoolOnSkuRemove(skuId);
    }

    @Override
    public boolean isGiftECouponOrder(Order order) {
        return CardKeyTradeSupport.isGiftECouponOrder(order);
    }

    @Override
    public boolean isPintuanECouponOrder(Order order) {
        return CardKeyTradeSupport.isPintuanECouponOrder(order);
    }

    @Override
    public boolean isPurchaseTypeOrder(Order order) {
        return CardKeyTradeSupport.isPurchaseTypeOrder(order);
    }

    @Override
    public boolean isECouponFulfillmentEligible(Order order) {
        return CardKeyTradeSupport.isECouponFulfillmentEligible(order);
    }

    @Override
    public boolean isECouponOnlyTrade(TradeDTO tradeDTO) {
        return CardKeyTradeSupport.isECouponOnlyTrade(tradeDTO);
    }

    @Override
    public void normalizeECouponTrade(TradeDTO tradeDTO) {
        CardKeyTradeSupport.normalizeECouponTrade(tradeDTO);
    }

    @Override
    public void validateTradeForECoupon(TradeDTO tradeDTO) {
        CardKeyTradeSupport.validateTradeForECoupon(tradeDTO, storeService, () -> {
            List<cn.lili.modules.order.cart.entity.vo.CartSkuVO> checked = tradeDTO.getCheckedSkuList();
            return (checked != null && !checked.isEmpty() && checked.get(0).getGoodsSku() != null)
                    ? countUnusedPoolStock(checked.get(0).getGoodsSku().getId()) : 0;
        });
    }

    @Override
    public void validateAddToCart(String skuId, Integer num, String cartType) {
        CardKeyTradeSupport.validateAddToCart(skuId, num, cartType, goodsSkuService, storeService, () -> countUnusedPoolStock(skuId));
    }

    @Override
    public void validatePayOrder(Order order) {
        CardKeyTradeSupport.validatePayOrder(order, storeService);
    }

    @Override
    public void enrichOrderDetail(OrderDetailVO orderDetailVO) {
        CardKeyTradeSupport.enrichOrderDetail(this, orderDetailVO, orderMapper, orderItemService, cardKeySecretUtil);
    }

    @Override
    public List<CardKeyVO> listCardKeyVOByOrderItemSn(String orderItemSn, boolean includeStatus) {
        return CardKeyTradeSupport.listCardKeyVOByOrderItemSn(this, cardKeySecretUtil, orderItemSn, includeStatus);
    }

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

    public boolean alreadyBound(String orderSn, CardKeyStatusEnum status) {
        return this.count(new LambdaQueryWrapper<CardKey>()
                .eq(CardKey::getOrderSn, orderSn)
                .eq(CardKey::getStatus, status.name())
                .eq(CardKey::getDeleteFlag, false)) > 0;
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

    private String currentStoreId() {
        AuthUser user = UserContext.getCurrentUser();
        if (user == null || CharSequenceUtil.isBlank(user.getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        return user.getStoreId();
    }
}
