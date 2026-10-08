package cn.lili.modules.appointment.serviceimpl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.appointment.entity.dos.*;
import cn.lili.modules.appointment.entity.dto.*;
import cn.lili.modules.appointment.entity.dto.search.AppointmentWorkOrderSearchParams;
import cn.lili.modules.appointment.entity.enums.*;
import cn.lili.modules.appointment.entity.vo.AppointmentBuyerOrderDetailVO;
import cn.lili.modules.appointment.entity.vo.AppointmentCalendarDaySummaryVO;
import cn.lili.modules.appointment.entity.vo.AppointmentCalendarEntryVO;
import cn.lili.modules.appointment.entity.vo.AppointmentGoodsExtVO;
import cn.lili.modules.appointment.entity.vo.AppointmentSlotStockVO;
import cn.lili.modules.appointment.entity.vo.AppointmentVerificationCodeVO;
import cn.lili.modules.appointment.entity.vo.AppointmentVerificationOrderVO;
import cn.lili.modules.appointment.entity.vo.AppointmentVerificationResultVO;
import cn.lili.modules.appointment.entity.vo.AppointmentWorkOrderVO;
import cn.lili.modules.appointment.mapper.*;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.appointment.support.AppointmentCheckoutContextHolder;
import cn.lili.modules.appointment.support.AppointmentFormAnswerSupport;
import cn.lili.modules.appointment.support.AppointmentGoodsPublishSupport;
import cn.lili.modules.appointment.support.AppointmentNoticeSupport;
import cn.lili.modules.appointment.util.AppointmentGeoFence;
import cn.lili.modules.appointment.util.AppointmentSnUtil;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum;
import cn.lili.modules.order.order.entity.enums.RefundStatusEnum;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.payment.kit.Payment;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.AppointmentOrderSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.vo.OrderVO;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 预约服务商品核心服务实现
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Slf4j
@Service
public class AppointmentServiceImpl extends ServiceImpl<AppointmentGoodsExtMapper, AppointmentGoodsExt>
        implements AppointmentService {

    private static final String FULFILL_FAIL_CANCEL_REASON = "预约时段库存不足，支付失败已退款";

    @Autowired
    private AppointmentSlotTemplateMapper slotTemplateMapper;
    @Autowired
    private AppointmentSlotStockMapper slotStockMapper;
    @Autowired
    private AppointmentOrderExtMapper orderExtMapper;
    @Autowired
    private AppointmentServiceStaffMapper serviceStaffMapper;
    @Autowired
    private AppointmentWorkOrderMapper workOrderMapper;
    @Autowired
    private AppointmentVerificationCodeMapper verificationCodeMapper;
    @Autowired
    private AppointmentStoreConfigMapper storeConfigMapper;
    @Autowired
    private GoodsSkuService goodsSkuService;
    @Autowired
    private GoodsService goodsService;
    @Autowired
    private StoreService storeService;
    @Autowired
    private OrderMapper orderMapper;
    @Autowired
    private OrderItemService orderItemService;
    @Autowired
    private OrderService orderService;
    @Autowired
    private AppointmentSystemFormMapper systemFormMapper;
    @Autowired
    private AppointmentNoticeSupport appointmentNoticeSupport;
    @Autowired
    private SettingService settingService;

    @Override
    public void validateAddToCart(String skuId, Integer num, String cartType) {
        GoodsSku sku = goodsSkuService.getById(skuId);
        if (sku == null || !GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(sku.getGoodsType())) {
            return;
        }
        if (CartTypeEnum.CART.name().equals(cartType)) {
            throw new ServiceException(ResultCode.APPOINTMENT_CART_FORBIDDEN);
        }
        if (!CartTypeEnum.BUY_NOW.name().equals(cartType)) {
            throw new ServiceException(ResultCode.APPOINTMENT_BUY_NOW_REQUIRED);
        }
    }

    @Override
    public void validateTrade(TradeDTO tradeDTO) {
        List<CartSkuVO> checked = tradeDTO.getCheckedSkuList();
        if (CollUtil.isEmpty(checked)) {
            return;
        }
        long appointmentCount = checked.stream()
                .filter(s -> s.getGoodsSku() != null
                        && GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(s.getGoodsSku().getGoodsType()))
                .count();
        if (appointmentCount == 0) {
            return;
        }
        boolean hasNonAppointment = checked.stream()
                .anyMatch(s -> s.getGoodsSku() == null
                        || !GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(s.getGoodsSku().getGoodsType()));
        if (hasNonAppointment) {
            throw new ServiceException(ResultCode.APPOINTMENT_MIXED_CHECKOUT);
        }
        if (checked.size() > 1) {
            throw new ServiceException(ResultCode.APPOINTMENT_SINGLE_SKU_ONLY);
        }
        if (!CartTypeEnum.BUY_NOW.equals(tradeDTO.getCartTypeEnum())) {
            throw new ServiceException(ResultCode.APPOINTMENT_BUY_NOW_REQUIRED);
        }
        AppointmentCheckoutDTO checkout = tradeDTO.getAppointmentCheckout();
        if (checkout == null) {
            checkout = AppointmentCheckoutContextHolder.get(tradeDTO.getMemberId());
        }
        if (checkout == null) {
            throw new ServiceException(ResultCode.APPOINTMENT_CHECKOUT_CONTEXT_MISSING);
        }
    }

    @Override
    public void validatePayOrder(Order order) {
        if (order == null || !OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            return;
        }
        Store store = storeService.getById(order.getStoreId());
        if (store == null) {
            throw new ServiceException(ResultCode.STORE_NOT_EXIST);
        }
    }

    @Override
    public boolean isAppointmentOnlyTrade(TradeDTO tradeDTO) {
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
                && GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(line.getGoodsSku().getGoodsType()));
    }

    @Override
    public void normalizeAppointmentTrade(TradeDTO tradeDTO) {
        if (!isAppointmentOnlyTrade(tradeDTO)) {
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
    public AppointmentGoodsExtVO getGoodsExt(String goodsId) {
        AppointmentGoodsExt ext = this.getOne(new LambdaQueryWrapper<AppointmentGoodsExt>()
                .eq(AppointmentGoodsExt::getGoodsId, goodsId)
                .eq(AppointmentGoodsExt::getDeleteFlag, false));
        if (ext == null) {
            return null;
        }
        AppointmentGoodsExtVO vo = BeanUtil.copyProperties(ext, AppointmentGoodsExtVO.class);
        List<AppointmentSlotTemplate> templates = slotTemplateMapper.selectList(
                new LambdaQueryWrapper<AppointmentSlotTemplate>()
                        .eq(AppointmentSlotTemplate::getGoodsId, goodsId)
                        .eq(AppointmentSlotTemplate::getDeleteFlag, false)
                        .orderByAsc(AppointmentSlotTemplate::getSortOrder));
        vo.setSlotTemplates(templates);
        vo.setServiceAreaRequired(isServiceAreaEnabled(ext.getStoreId()));
        if (CharSequenceUtil.isBlank(vo.getSystemFormSnapshot())
                && CharSequenceUtil.isNotBlank(vo.getSystemFormId())) {
            AppointmentSystemForm form = systemFormMapper.selectById(vo.getSystemFormId());
            if (form != null) {
                vo.setSystemFormSnapshot(form.getFormSchema());
            }
        }
        return vo;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveGoodsExt(AppointmentGoodsExtSaveDTO dto) {
        AuthUser user = UserContext.getCurrentUser();
        if (user == null || CharSequenceUtil.isBlank(user.getStoreId())) {
            throw new ServiceException(ResultCode.STORE_NOT_LOGIN_ERROR);
        }
        Goods goods = goodsService.getById(dto.getGoodsId());
        if (goods == null || !GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(goods.getGoodsType())) {
            throw new ServiceException(ResultCode.APPOINTMENT_GOODS_TYPE_REQUIRED);
        }
        if (!user.getStoreId().equals(goods.getStoreId())) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST_STORE);
        }
        persistGoodsExt(goods, dto);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void syncGoodsExtFromPublish(String goodsId, GoodsOperationDTO goodsOperationDTO) {
        if (goodsOperationDTO == null
                || !GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(goodsOperationDTO.getGoodsType())) {
            return;
        }
        if (goodsOperationDTO.getAppointmentSettings() == null
                && goodsOperationDTO.getAppointmentSlotTemplate() == null) {
            return;
        }
        Goods goods = goodsService.getById(goodsId);
        if (goods == null) {
            return;
        }
        List<GoodsSku> skus = goodsSkuService.listByGoodsIdAndStoreId(goodsId, goods.getStoreId());
        AppointmentGoodsExtSaveDTO saveDTO = AppointmentGoodsPublishSupport.toSaveDto(
                goodsId,
                goodsOperationDTO.getAppointmentSettings(),
                goodsOperationDTO.getAppointmentSlotTemplate(),
                goodsOperationDTO.getSkuList(),
                skus);
        persistGoodsExt(goods, saveDTO);
    }

    @Override
    public void enrichGoodsVoForSeller(GoodsVO goodsVO) {
        if (goodsVO == null || !GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(goodsVO.getGoodsType())) {
            return;
        }
        AppointmentGoodsExt ext = this.getOne(new LambdaQueryWrapper<AppointmentGoodsExt>()
                .eq(AppointmentGoodsExt::getGoodsId, goodsVO.getId())
                .eq(AppointmentGoodsExt::getDeleteFlag, false));
        List<AppointmentSlotTemplate> templates = slotTemplateMapper.selectList(
                new LambdaQueryWrapper<AppointmentSlotTemplate>()
                        .eq(AppointmentSlotTemplate::getGoodsId, goodsVO.getId())
                        .eq(AppointmentSlotTemplate::getDeleteFlag, false)
                        .orderByAsc(AppointmentSlotTemplate::getSortOrder));
        goodsVO.setAppointmentSettings(AppointmentGoodsPublishSupport.toSettingsMap(ext));
        goodsVO.setAppointmentSlotTemplate(AppointmentGoodsPublishSupport.toSlotTemplateMap(ext, templates));
        AppointmentGoodsPublishSupport.attachSkuSlotCapacities(goodsVO, templates);
    }

    private void persistGoodsExt(Goods goods, AppointmentGoodsExtSaveDTO dto) {
        AppointmentGoodsExt ext = this.getOne(new LambdaQueryWrapper<AppointmentGoodsExt>()
                .eq(AppointmentGoodsExt::getGoodsId, dto.getGoodsId())
                .eq(AppointmentGoodsExt::getDeleteFlag, false));
        if (ext == null) {
            ext = BeanUtil.copyProperties(dto, AppointmentGoodsExt.class);
            ext.setId(SnowFlake.getIdStr());
            ext.setStoreId(goods.getStoreId());
            ext.setDeleteFlag(false);
            fillSystemFormSnapshot(ext, goods.getStoreId());
            this.save(ext);
        } else {
            BeanUtil.copyProperties(dto, ext, "id", "goodsId", "storeId");
            fillSystemFormSnapshot(ext, goods.getStoreId());
            this.updateById(ext);
        }
        if (CollUtil.isNotEmpty(dto.getSlotTemplates())) {
            slotTemplateMapper.delete(new LambdaQueryWrapper<AppointmentSlotTemplate>()
                    .eq(AppointmentSlotTemplate::getGoodsId, dto.getGoodsId()));
            for (AppointmentSlotTemplateDTO templateDTO : dto.getSlotTemplates()) {
                AppointmentSlotTemplate template = BeanUtil.copyProperties(templateDTO, AppointmentSlotTemplate.class);
                template.setId(SnowFlake.getIdStr());
                template.setGoodsId(dto.getGoodsId());
                template.setStoreId(goods.getStoreId());
                template.setDeleteFlag(false);
                if (template.getEnabled() == null) {
                    template.setEnabled(true);
                }
                if (template.getCapacity() == null) {
                    template.setCapacity(0);
                }
                slotTemplateMapper.insert(template);
            }
        }
    }

    @Override
    public List<AppointmentSlotStockVO> listAvailableSlots(String skuId, Date slotDate) {
        GoodsSku sku = goodsSkuService.getById(skuId);
        if (sku == null || !GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(sku.getGoodsType())) {
            throw new ServiceException(ResultCode.APPOINTMENT_SKU_NOT_FOUND);
        }
        AppointmentGoodsExt ext = this.getOne(new LambdaQueryWrapper<AppointmentGoodsExt>()
                .eq(AppointmentGoodsExt::getGoodsId, sku.getGoodsId())
                .eq(AppointmentGoodsExt::getDeleteFlag, false));
        if (!isBookableSlotDate(ext, slotDate)) {
            return Collections.emptyList();
        }
        ensureSlotStockForDate(sku, ext, slotDate);
        List<AppointmentSlotStock> stocks = slotStockMapper.selectList(new LambdaQueryWrapper<AppointmentSlotStock>()
                .eq(AppointmentSlotStock::getSkuId, skuId)
                .eq(AppointmentSlotStock::getSlotDate, slotDate)
                .eq(AppointmentSlotStock::getDeleteFlag, false)
                .orderByAsc(AppointmentSlotStock::getSlotStartTime));
        boolean showRemaining = ext == null || !Boolean.FALSE.equals(ext.getShowRemainingQty());
        return stocks.stream()
                .filter(stock -> isSlotStartAfterNow(stock.getSlotDate(), stock.getSlotStartTime()))
                .map(stock -> {
            AppointmentSlotStockVO vo = BeanUtil.copyProperties(stock, AppointmentSlotStockVO.class);
            vo.setShowRemainingQty(showRemaining);
            return vo;
        }).collect(Collectors.toList());
    }

    @Override
    public void bindCheckoutContext(TradeDTO tradeDTO, AppointmentCheckoutDTO checkout) {
        if (tradeDTO == null || checkout == null) {
            throw new ServiceException(ResultCode.APPOINTMENT_CHECKOUT_CONTEXT_MISSING);
        }
        validateAndEnrichCheckout(tradeDTO, checkout);
        tradeDTO.setAppointmentCheckout(checkout);
        AppointmentCheckoutContextHolder.bind(tradeDTO.getMemberId(), checkout);
    }

    @Override
    public void stashCheckoutForPayment(TradeDTO tradeDTO) {
        if (!isAppointmentOnlyTrade(tradeDTO) || CollUtil.isEmpty(tradeDTO.getOrderVO())) {
            return;
        }
        AppointmentCheckoutDTO checkout = resolveCheckout(tradeDTO);
        if (checkout == null) {
            throw new ServiceException(ResultCode.APPOINTMENT_CHECKOUT_CONTEXT_MISSING);
        }
        tradeDTO.setAppointmentCheckout(checkout);
        for (OrderVO orderVO : tradeDTO.getOrderVO()) {
            if (orderVO == null || CharSequenceUtil.isBlank(orderVO.getSn())) {
                continue;
            }
            persistOrderExt(orderVO.getSn(), checkout);
        }
        AppointmentCheckoutContextHolder.remove(tradeDTO.getMemberId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void persistOrderExtFromTrade(TradeDTO tradeDTO) {
        if (tradeDTO == null || !isAppointmentOnlyTrade(tradeDTO) || CollUtil.isEmpty(tradeDTO.getOrderVO())) {
            return;
        }
        AppointmentCheckoutDTO checkout = resolveCheckout(tradeDTO);
        for (OrderVO orderVO : tradeDTO.getOrderVO()) {
            if (orderVO == null || CharSequenceUtil.isBlank(orderVO.getSn())) {
                continue;
            }
            persistOrderExt(orderVO.getSn(), checkout);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void persistOrderExt(String orderSn) {
        persistOrderExt(orderSn, null);
    }

    private void persistOrderExt(String orderSn, AppointmentCheckoutDTO checkoutHint) {
        Order order = orderMapper.selectForUpdateBySn(orderSn);
        if (order == null || !OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            return;
        }
        if (orderExtMapper.selectCount(new LambdaQueryWrapper<AppointmentOrderExt>()
                .eq(AppointmentOrderExt::getOrderSn, orderSn)) > 0) {
            return;
        }
        AppointmentCheckoutDTO checkout = checkoutHint != null ? checkoutHint : resolveCheckoutFromOrder(order);
        if (checkout == null) {
            throw new ServiceException(ResultCode.APPOINTMENT_CHECKOUT_CONTEXT_MISSING);
        }
        List<OrderItem> items = orderItemService.getByOrderSn(orderSn);
        if (CollUtil.isEmpty(items)) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        OrderItem item = items.get(0);
        AppointmentSlotStock stock = slotStockMapper.selectById(checkout.getSlotStockId());
        if (stock == null || Boolean.TRUE.equals(stock.getDeleteFlag())
                || !item.getSkuId().equals(stock.getSkuId())) {
            throw new ServiceException(ResultCode.APPOINTMENT_SLOT_NOT_FOUND);
        }
        AppointmentGoodsExt goodsExt = this.getOne(new LambdaQueryWrapper<AppointmentGoodsExt>()
                .eq(AppointmentGoodsExt::getGoodsId, item.getGoodsId())
                .eq(AppointmentGoodsExt::getDeleteFlag, false));
        AppointmentStoreConfig storeConfig = storeConfigMapper.selectOne(new LambdaQueryWrapper<AppointmentStoreConfig>()
                .eq(AppointmentStoreConfig::getStoreId, order.getStoreId())
                .eq(AppointmentStoreConfig::getServiceMode, checkout.getServiceMode())
                .eq(AppointmentStoreConfig::getDeleteFlag, false));
        int quantity = item.getNum() == null ? 1 : item.getNum();
        AppointmentOrderExt ext = new AppointmentOrderExt();
        ext.setId(SnowFlake.getIdStr());
        ext.setOrderSn(orderSn);
        ext.setGoodsId(item.getGoodsId());
        ext.setSkuId(item.getSkuId());
        ext.setStoreId(order.getStoreId());
        ext.setMemberId(order.getMemberId());
        ext.setServiceMode(checkout.getServiceMode());
        ext.setSlotStockId(stock.getId());
        ext.setSlotDate(stock.getSlotDate());
        ext.setSlotStartTime(stock.getSlotStartTime());
        ext.setSlotEndTime(stock.getSlotEndTime());
        ext.setQuantity(quantity);
        ext.setContactName(checkout.getContactName());
        ext.setContactPhone(checkout.getContactPhone());
        ext.setServiceAddressSnapshot(checkout.getServiceAddressSnapshot());
        ext.setFormAnswerJson(buildFormAnswerJson(checkout, goodsExt, quantity));
        ext.setCancelPolicySnapshot(buildCancelPolicySnapshot(goodsExt));
        ext.setWorkOrderVisibleSnapshot(storeConfig == null || !Boolean.FALSE.equals(storeConfig.getWorkOrderVisible()));
        ext.setRefundedAmount(0D);
        ext.setDeleteFlag(false);
        orderExtMapper.insert(ext);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void fulfillAfterPayment(String orderSn) {
        Order order = orderMapper.selectForUpdateBySn(orderSn);
        if (order == null || !OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            return;
        }
        if (!PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
            log.warn("预约履约跳过，订单未支付，orderSn={}", orderSn);
            return;
        }
        if (OrderStatusEnum.TAKE.name().equals(order.getOrderStatus())
                || OrderStatusEnum.COMPLETED.name().equals(order.getOrderStatus())) {
            log.info("预约履约跳过，订单已履约，orderSn={}", orderSn);
            return;
        }
        try {
            persistOrderExt(orderSn);
        } catch (ServiceException e) {
            log.error("预约履约失败，结算上下文缺失，orderSn={}", orderSn, e);
            failFulfillment(orderSn, null, 0);
            return;
        }
        AppointmentOrderExt orderExt = orderExtMapper.selectOne(new LambdaQueryWrapper<AppointmentOrderExt>()
                .eq(AppointmentOrderExt::getOrderSn, orderSn)
                .eq(AppointmentOrderExt::getDeleteFlag, false));
        if (orderExt == null) {
            log.error("预约履约失败，订单扩展缺失，orderSn={}", orderSn);
            failFulfillment(orderSn, null, 0);
            return;
        }
        if (workOrderMapper.selectCount(new LambdaQueryWrapper<AppointmentWorkOrder>()
                .eq(AppointmentWorkOrder::getOrderSn, orderSn)) > 0) {
            transitionOrderToTake(orderSn);
            return;
        }
        int quantity = orderExt.getQuantity() == null ? 1 : orderExt.getQuantity();
        if (!deductSlotStock(orderExt.getSlotStockId(), quantity)) {
            failFulfillment(orderSn, orderExt.getSlotStockId(), quantity);
            throw new ServiceException(ResultCode.APPOINTMENT_SLOT_INSUFFICIENT);
        }
        try {
            createWorkOrderAndCodes(order, orderExt, quantity);
            transitionOrderToTake(orderSn);
            notifyBookingSuccess(orderSn);
        } catch (Exception e) {
            log.error("预约履约失败，orderSn={}", orderSn, e);
            restoreSlotStock(orderExt.getSlotStockId(), quantity);
            failFulfillment(orderSn, orderExt.getSlotStockId(), quantity);
            throw e;
        }
    }

    @Override
    public boolean deductSlotStock(String slotStockId, int quantity) {
        if (CharSequenceUtil.isBlank(slotStockId) || quantity <= 0) {
            return false;
        }
        slotStockMapper.selectForUpdateById(slotStockId);
        return slotStockMapper.deductAvailableQty(slotStockId, quantity) > 0;
    }

    @Override
    public void restoreSlotStock(String slotStockId, int quantity) {
        if (CharSequenceUtil.isBlank(slotStockId) || quantity <= 0) {
            return;
        }
        slotStockMapper.restoreAvailableQty(slotStockId, quantity);
    }

    @Override
    public IPage<AppointmentServiceStaff> pageStaff(String storeId, AppointmentWorkOrderSearchParams params) {
        Page<AppointmentServiceStaff> page = new Page<>(params.getPageNumber(), params.getPageSize());
        return serviceStaffMapper.selectPage(page, new LambdaQueryWrapper<AppointmentServiceStaff>()
                .eq(AppointmentServiceStaff::getStoreId, storeId)
                .eq(AppointmentServiceStaff::getDeleteFlag, false)
                .orderByAsc(AppointmentServiceStaff::getSortOrder)
                .orderByDesc(AppointmentServiceStaff::getCreateTime));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public AppointmentServiceStaff saveStaff(String storeId, AppointmentServiceStaffDTO dto) {
        AppointmentServiceStaff occupied = findStaffByClerk(storeId, dto.getClerkUserId());
        if (CharSequenceUtil.isNotBlank(dto.getId())) {
            AppointmentServiceStaff staff = serviceStaffMapper.selectById(dto.getId());
            if (staff == null || !storeId.equals(staff.getStoreId()) || Boolean.TRUE.equals(staff.getDeleteFlag())) {
                throw new ServiceException(ResultCode.APPOINTMENT_STAFF_NOT_FOUND);
            }
            releaseDeletedClerk(occupied, staff.getId());
            BeanUtil.copyProperties(dto, staff, "id", "storeId");
            staff.setDeleteFlag(false);
            serviceStaffMapper.updateById(staff);
            return staff;
        }
        if (occupied != null) {
            if (!Boolean.TRUE.equals(occupied.getDeleteFlag())) {
                throw new ServiceException(ResultCode.APPOINTMENT_STAFF_EXISTS);
            }
            BeanUtil.copyProperties(dto, occupied, "id", "storeId", "clerkUserId");
            occupied.setDeleteFlag(false);
            if (CharSequenceUtil.isBlank(occupied.getStatus())) {
                occupied.setStatus("ENABLED");
            }
            serviceStaffMapper.updateById(occupied);
            return occupied;
        }
        AppointmentServiceStaff staff = BeanUtil.copyProperties(dto, AppointmentServiceStaff.class);
        staff.setId(SnowFlake.getIdStr());
        staff.setStoreId(storeId);
        staff.setDeleteFlag(false);
        if (CharSequenceUtil.isBlank(staff.getStatus())) {
            staff.setStatus("ENABLED");
        }
        serviceStaffMapper.insert(staff);
        return staff;
    }

    private AppointmentServiceStaff findStaffByClerk(String storeId, String clerkUserId) {
        return serviceStaffMapper.selectOne(new LambdaQueryWrapper<AppointmentServiceStaff>()
                .eq(AppointmentServiceStaff::getStoreId, storeId)
                .eq(AppointmentServiceStaff::getClerkUserId, clerkUserId)
                .last("LIMIT 1"));
    }

    /**
     * 换绑店员时，清掉已删除的同店员记录，避免唯一约束挡住更新。
     */
    private void releaseDeletedClerk(AppointmentServiceStaff occupied, String currentId) {
        if (occupied == null || occupied.getId().equals(currentId)) {
            return;
        }
        if (Boolean.TRUE.equals(occupied.getDeleteFlag())) {
            serviceStaffMapper.deleteById(occupied.getId());
            return;
        }
        throw new ServiceException(ResultCode.APPOINTMENT_STAFF_EXISTS);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteStaff(String storeId, String staffId) {
        AppointmentServiceStaff staff = serviceStaffMapper.selectById(staffId);
        if (staff == null || !storeId.equals(staff.getStoreId())) {
            throw new ServiceException(ResultCode.APPOINTMENT_STAFF_NOT_FOUND);
        }
        staff.setDeleteFlag(true);
        serviceStaffMapper.updateById(staff);
    }

    @Override
    public List<AppointmentStoreConfig> listStoreConfig(String storeId) {
        return storeConfigMapper.selectList(new LambdaQueryWrapper<AppointmentStoreConfig>()
                .eq(AppointmentStoreConfig::getStoreId, storeId)
                .eq(AppointmentStoreConfig::getDeleteFlag, false));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveStoreConfig(String storeId, AppointmentStoreConfigDTO dto) {
        AppointmentStoreConfig config = storeConfigMapper.selectOne(new LambdaQueryWrapper<AppointmentStoreConfig>()
                .eq(AppointmentStoreConfig::getStoreId, storeId)
                .eq(AppointmentStoreConfig::getServiceMode, dto.getServiceMode()));
        if (config == null) {
            config = BeanUtil.copyProperties(dto, AppointmentStoreConfig.class);
            config.setId(SnowFlake.getIdStr());
            config.setStoreId(storeId);
            config.setDeleteFlag(false);
            storeConfigMapper.insert(config);
        } else {
            BeanUtil.copyProperties(dto, config, "id", "storeId", "serviceMode");
            storeConfigMapper.updateById(config);
        }
    }

    @Override
    public IPage<AppointmentWorkOrderVO> pageWorkOrders(AppointmentWorkOrderSearchParams params, boolean managerScope) {
        Page<AppointmentWorkOrder> page = new Page<>(params.getPageNumber(), params.getPageSize());
        LambdaQueryWrapper<AppointmentWorkOrder> wrapper = new LambdaQueryWrapper<AppointmentWorkOrder>()
                .eq(AppointmentWorkOrder::getDeleteFlag, false)
                .orderByDesc(AppointmentWorkOrder::getCreateTime);
        if (!managerScope && CharSequenceUtil.isNotBlank(params.getStoreId())) {
            wrapper.eq(AppointmentWorkOrder::getStoreId, params.getStoreId());
        }
        if ("OPEN".equals(params.getServiceStatus())) {
            wrapper.in(AppointmentWorkOrder::getServiceStatus, AppointmentWorkOrderStatusEnum.unfinishedNames());
        } else if (CharSequenceUtil.isNotBlank(params.getServiceStatus())) {
            wrapper.eq(AppointmentWorkOrder::getServiceStatus, params.getServiceStatus());
        }
        if (CharSequenceUtil.isNotBlank(params.getWorkOrderSn())) {
            wrapper.like(AppointmentWorkOrder::getWorkOrderSn, params.getWorkOrderSn());
        }
        if (CharSequenceUtil.isNotBlank(params.getOrderSn())) {
            wrapper.like(AppointmentWorkOrder::getOrderSn, params.getOrderSn());
        }
        if (CharSequenceUtil.isNotBlank(params.getServiceMode())) {
            wrapper.eq(AppointmentWorkOrder::getServiceMode, params.getServiceMode());
        }
        if (CharSequenceUtil.isNotBlank(params.getMemberId())) {
            wrapper.eq(AppointmentWorkOrder::getMemberId, params.getMemberId());
        }
        if (params.getAppointmentDateStart() != null) {
            wrapper.ge(AppointmentWorkOrder::getAppointmentTime, params.getAppointmentDateStart());
        }
        if (params.getAppointmentDateEnd() != null) {
            wrapper.le(AppointmentWorkOrder::getAppointmentTime, DateUtil.endOfDay(params.getAppointmentDateEnd()));
        }
        IPage<AppointmentWorkOrder> result = workOrderMapper.selectPage(page, wrapper);
        Map<String, String> staffNameMap = loadStaffNameMap(result.getRecords());
        Page<AppointmentWorkOrderVO> voPage = new Page<>(result.getCurrent(), result.getSize(), result.getTotal());
        voPage.setRecords(result.getRecords().stream().map(record -> {
            AppointmentWorkOrderVO vo = BeanUtil.copyProperties(record, AppointmentWorkOrderVO.class);
            if (CharSequenceUtil.isNotBlank(record.getStaffId())) {
                vo.setStaffName(staffNameMap.get(record.getStaffId()));
            }
            if (managerScope) {
                Store store = storeService.getById(record.getStoreId());
                if (store != null) {
                    vo.setStoreName(store.getStoreName());
                }
            }
            return vo;
        }).collect(Collectors.toList()));
        return voPage;
    }

    @Override
    public List<AppointmentServiceStaff> listEnabledStaff(String storeId) {
        return serviceStaffMapper.selectList(new LambdaQueryWrapper<AppointmentServiceStaff>()
                .eq(AppointmentServiceStaff::getStoreId, storeId)
                .eq(AppointmentServiceStaff::getStatus, "ENABLED")
                .eq(AppointmentServiceStaff::getDeleteFlag, false)
                .orderByAsc(AppointmentServiceStaff::getSortOrder)
                .orderByDesc(AppointmentServiceStaff::getCreateTime));
    }

    @Override
    public AppointmentVerificationOrderVO getOrderByVerificationCode(String storeId, String code) {
        AppointmentVerificationCode verificationCode = loadVerificationCode(storeId, code);
        return buildVerificationOrderVO(verificationCode.getOrderSn(), storeId);
    }

    @Override
    public AppointmentVerificationOrderVO getVerificationByOrderSn(String storeId, String orderSn) {
        Order order = orderMapper.selectOne(new LambdaQueryWrapper<Order>()
                .eq(Order::getSn, orderSn)
                .eq(Order::getStoreId, storeId));
        if (order == null || !OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        return buildVerificationOrderVO(orderSn, storeId);
    }

    @Override
    public AppointmentBuyerOrderDetailVO getBuyerOrderDetail(String memberId, String orderSn) {
        Order order = orderMapper.selectOne(new LambdaQueryWrapper<Order>()
                .eq(Order::getSn, orderSn)
                .eq(Order::getMemberId, memberId));
        if (order == null || !OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        AppointmentOrderExt orderExt = orderExtMapper.selectOne(new LambdaQueryWrapper<AppointmentOrderExt>()
                .eq(AppointmentOrderExt::getOrderSn, orderSn)
                .eq(AppointmentOrderExt::getDeleteFlag, false));
        if (orderExt == null) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        AppointmentWorkOrder workOrder = workOrderMapper.selectOne(new LambdaQueryWrapper<AppointmentWorkOrder>()
                .eq(AppointmentWorkOrder::getOrderSn, orderSn)
                .eq(AppointmentWorkOrder::getDeleteFlag, false));
        List<AppointmentVerificationCode> orderCodes = listOrderVerificationCodes(orderSn);

        AppointmentBuyerOrderDetailVO vo = new AppointmentBuyerOrderDetailVO();
        vo.setOrderSn(order.getSn());
        vo.setOrderStatus(order.getOrderStatus());
        vo.setGoodsId(orderExt.getGoodsId());
        vo.setSkuId(orderExt.getSkuId());
        vo.setServiceMode(orderExt.getServiceMode());
        vo.setSlotDate(orderExt.getSlotDate());
        vo.setSlotStartTime(orderExt.getSlotStartTime());
        vo.setSlotEndTime(orderExt.getSlotEndTime());
        vo.setQuantity(orderExt.getQuantity());
        vo.setContactName(orderExt.getContactName());
        vo.setContactPhone(orderExt.getContactPhone());
        vo.setServiceAddressSnapshot(orderExt.getServiceAddressSnapshot());
        vo.setCancelPolicySnapshot(orderExt.getCancelPolicySnapshot());
        vo.setFormAnswerJson(orderExt.getFormAnswerJson());
        if (workOrder != null) {
            vo.setWorkOrderStatus(workOrder.getServiceStatus());
        }
        vo.setCodes(orderCodes.stream()
                .map(code -> BeanUtil.copyProperties(code, AppointmentVerificationCodeVO.class))
                .collect(Collectors.toList()));
        return vo;
    }

    @Override
    public AppointmentBuyerOrderDetailVO getOrderExtDetail(String orderSn) {
        Order order = orderMapper.selectOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, orderSn));
        if (order == null || !OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        return getBuyerOrderDetail(order.getMemberId(), orderSn);
    }

    @Override
    public boolean hasVerificationCode(String storeId, String code) {
        if (CharSequenceUtil.isBlank(storeId) || CharSequenceUtil.isBlank(code)) {
            return false;
        }
        Long count = verificationCodeMapper.selectCount(new LambdaQueryWrapper<AppointmentVerificationCode>()
                .eq(AppointmentVerificationCode::getStoreId, storeId)
                .eq(AppointmentVerificationCode::getCode, code.trim())
                .eq(AppointmentVerificationCode::getDeleteFlag, false));
        return count != null && count > 0;
    }

    @Override
    public AppointmentVerificationCode findVerificationCode(String storeId, String code) {
        if (CharSequenceUtil.isBlank(storeId) || CharSequenceUtil.isBlank(code)) {
            return null;
        }
        return verificationCodeMapper.selectOne(new LambdaQueryWrapper<AppointmentVerificationCode>()
                .eq(AppointmentVerificationCode::getStoreId, storeId)
                .eq(AppointmentVerificationCode::getCode, code.trim())
                .eq(AppointmentVerificationCode::getDeleteFlag, false)
                .last("LIMIT 1"));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public AppointmentVerificationResultVO verifyCodeByOperator(String storeId, String userId, String code) {
        return doVerifyCode(storeId, userId, code, null);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public AppointmentVerificationResultVO verifyCode(String storeId, String userId, AppointmentVerifyCodeDTO dto) {
        if (dto == null || CharSequenceUtil.isBlank(dto.getCode())) {
            throw new ServiceException(ResultCode.APPOINTMENT_CODE_NOT_FOUND);
        }
        return doVerifyCode(storeId, userId, dto.getCode(), dto.getStaffId());
    }

    private AppointmentVerificationResultVO doVerifyCode(String storeId, String userId, String code, String staffId) {
        boolean withStaff = CharSequenceUtil.isNotBlank(staffId);
        if (withStaff) {
            validateEnabledStaff(storeId, staffId);
        }
        AppointmentVerificationCode verificationCode = loadVerificationCode(storeId, code);
        if (!AppointmentVerificationCodeStatusEnum.UNUSED.name().equals(verificationCode.getStatus())) {
            throw new ServiceException(ResultCode.APPOINTMENT_CODE_ALREADY_USED);
        }

        Order order = orderMapper.selectForUpdateBySn(verificationCode.getOrderSn());
        validateAppointmentOrderForVerify(order, storeId);
        if (withStaff) {
            enforceSameStaffForOrder(verificationCode.getOrderSn(), staffId);
        }

        Date now = new Date();
        verificationCode.setStatus(AppointmentVerificationCodeStatusEnum.USED.name());
        verificationCode.setVerifiedTime(now);
        verificationCode.setVerifiedByUserId(userId);
        if (withStaff) {
            verificationCode.setStaffId(staffId);
        }
        verificationCodeMapper.updateById(verificationCode);

        List<AppointmentVerificationCode> orderCodes = listOrderVerificationCodes(verificationCode.getOrderSn());
        int totalCount = orderCodes.size();
        int verifiedCount = countUsedCodes(orderCodes);
        AppointmentOrderExt orderExt = loadOrderExt(verificationCode.getOrderSn());
        AppointmentWorkOrder workOrder = loadWorkOrderByOrderSn(verificationCode.getOrderSn());

        AppointmentVerificationResultVO result = new AppointmentVerificationResultVO();
        result.setOrderSn(order.getSn());
        result.setCode(verificationCode.getCode());
        result.setSeqNo(verificationCode.getSeqNo());
        result.setTotalCount(totalCount);
        result.setVerifiedCount(verifiedCount);
        result.setAllVerified(verifiedCount >= totalCount);
        result.setServiceMode(orderExt.getServiceMode());
        result.setWorkOrderVisible(Boolean.TRUE.equals(orderExt.getWorkOrderVisibleSnapshot()));
        result.setWorkOrderId(workOrder.getId());
        result.setWorkOrderSn(workOrder.getWorkOrderSn());
        result.setOrderCompleted(false);
        result.setWorkOrderStatus(workOrder.getServiceStatus());

        if (verifiedCount < totalCount) {
            return result;
        }

        boolean inStoreVisible = AppointmentServiceModeEnum.IN_STORE.name().equals(orderExt.getServiceMode())
                && Boolean.TRUE.equals(orderExt.getWorkOrderVisibleSnapshot());
        if (inStoreVisible) {
            workOrder.setServiceStatus(AppointmentWorkOrderStatusEnum.IN_SERVICE.name());
            if (withStaff) {
                workOrder.setStaffId(staffId);
            }
            workOrderMapper.updateById(workOrder);
            result.setWorkOrderStatus(AppointmentWorkOrderStatusEnum.IN_SERVICE.name());
        } else {
            workOrder.setServiceStatus(AppointmentWorkOrderStatusEnum.SERVICE_ENDED.name());
            if (withStaff) {
                workOrder.setStaffId(staffId);
            }
            workOrderMapper.updateById(workOrder);
            orderService.complete(order.getSn());
            result.setOrderCompleted(true);
            result.setWorkOrderStatus(AppointmentWorkOrderStatusEnum.SERVICE_ENDED.name());
        }
        return result;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void endService(String storeId, String workOrderId) {
        AppointmentWorkOrder workOrder = loadStoreWorkOrder(storeId, workOrderId);
        if (!AppointmentWorkOrderStatusEnum.IN_SERVICE.name().equals(workOrder.getServiceStatus())) {
            throw new ServiceException(ResultCode.APPOINTMENT_WORK_ORDER_STATUS_ERROR);
        }
        Order order = orderMapper.selectForUpdateBySn(workOrder.getOrderSn());
        if (order == null || !OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        if (!OrderStatusEnum.TAKE.name().equals(order.getOrderStatus())) {
            throw new ServiceException(ResultCode.APPOINTMENT_ORDER_NOT_TAKE);
        }
        workOrder.setServiceStatus(AppointmentWorkOrderStatusEnum.SERVICE_ENDED.name());
        workOrderMapper.updateById(workOrder);
        orderService.complete(order.getSn());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void claimWorkOrder(String storeId, String userId, String workOrderId) {
        AppointmentWorkOrder workOrder = loadStoreWorkOrder(storeId, workOrderId);
        if (!AppointmentWorkOrderStatusEnum.PENDING_CLAIM.name().equals(workOrder.getServiceStatus())) {
            throw new ServiceException(ResultCode.APPOINTMENT_WORK_ORDER_STATUS_ERROR);
        }
        workOrder.setServiceStatus(AppointmentWorkOrderStatusEnum.CLAIMED.name());
        workOrderMapper.updateById(workOrder);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void reassignWorkOrder(String storeId, AppointmentReassignDTO dto, String workOrderId) {
        validateEnabledStaff(storeId, dto.getStaffId());
        AppointmentWorkOrder workOrder = loadStoreWorkOrder(storeId, workOrderId);
        String status = workOrder.getServiceStatus();
        if (!AppointmentWorkOrderStatusEnum.CLAIMED.name().equals(status)
                && !AppointmentWorkOrderStatusEnum.WAIT_VERIFY.name().equals(status)
                && !AppointmentWorkOrderStatusEnum.IN_SERVICE.name().equals(status)) {
            throw new ServiceException(ResultCode.APPOINTMENT_WORK_ORDER_STATUS_ERROR);
        }
        workOrder.setStaffId(dto.getStaffId());
        workOrderMapper.updateById(workOrder);
    }

    @Override
    public Map<String, Long> countWorkOrderStatus(String storeId) {
        List<AppointmentWorkOrder> records = workOrderMapper.selectList(new LambdaQueryWrapper<AppointmentWorkOrder>()
                .eq(AppointmentWorkOrder::getStoreId, storeId)
                .eq(AppointmentWorkOrder::getDeleteFlag, false));
        Map<String, Long> counts = new LinkedHashMap<>();
        counts.put("ALL", (long) records.size());
        for (AppointmentWorkOrderStatusEnum statusEnum : AppointmentWorkOrderStatusEnum.values()) {
            counts.put(statusEnum.name(), 0L);
        }
        for (AppointmentWorkOrder record : records) {
            counts.merge(record.getServiceStatus(), 1L, Long::sum);
        }
        return counts;
    }

    @Override
    public List<AppointmentCalendarDaySummaryVO> calendarMonthSummary(String storeId, String month) {
        Date monthStart = DateUtil.parse(month + "-01");
        Date monthEnd = DateUtil.endOfMonth(monthStart);
        List<AppointmentOrderExt> orderExts = orderExtMapper.selectList(new LambdaQueryWrapper<AppointmentOrderExt>()
                .eq(AppointmentOrderExt::getStoreId, storeId)
                .eq(AppointmentOrderExt::getDeleteFlag, false)
                .ge(AppointmentOrderExt::getSlotDate, monthStart)
                .le(AppointmentOrderExt::getSlotDate, monthEnd));
        Map<String, Integer> headcountByDate = new TreeMap<>();
        for (AppointmentOrderExt ext : orderExts) {
            String dateKey = DateUtil.formatDate(ext.getSlotDate());
            int qty = ext.getQuantity() == null ? 1 : ext.getQuantity();
            headcountByDate.merge(dateKey, qty, Integer::sum);
        }
        return headcountByDate.entrySet().stream().map(entry -> {
            AppointmentCalendarDaySummaryVO vo = new AppointmentCalendarDaySummaryVO();
            vo.setDate(entry.getKey());
            vo.setHeadcount(entry.getValue());
            return vo;
        }).collect(Collectors.toList());
    }

    @Override
    public List<AppointmentCalendarEntryVO> calendarDayEntries(String storeId, Date date, String status) {
        List<AppointmentOrderExt> orderExts = orderExtMapper.selectList(new LambdaQueryWrapper<AppointmentOrderExt>()
                .eq(AppointmentOrderExt::getStoreId, storeId)
                .eq(AppointmentOrderExt::getSlotDate, date)
                .eq(AppointmentOrderExt::getDeleteFlag, false)
                .orderByAsc(AppointmentOrderExt::getSlotStartTime));
        if (CollUtil.isEmpty(orderExts)) {
            return Collections.emptyList();
        }
        List<String> orderSns = orderExts.stream().map(AppointmentOrderExt::getOrderSn).collect(Collectors.toList());
        Map<String, Order> orderMap = orderMapper.selectList(new LambdaQueryWrapper<Order>()
                        .in(Order::getSn, orderSns))
                .stream().collect(Collectors.toMap(Order::getSn, o -> o, (a, b) -> a));
        Map<String, AppointmentWorkOrder> workOrderMap = workOrderMapper.selectList(new LambdaQueryWrapper<AppointmentWorkOrder>()
                        .in(AppointmentWorkOrder::getOrderSn, orderSns)
                        .eq(AppointmentWorkOrder::getDeleteFlag, false))
                .stream().collect(Collectors.toMap(AppointmentWorkOrder::getOrderSn, w -> w, (a, b) -> a));
        Map<String, String> staffNameMap = loadStaffNameMap(workOrderMap.values());

        List<AppointmentCalendarEntryVO> entries = new ArrayList<>();
        for (AppointmentOrderExt ext : orderExts) {
            Order order = orderMap.get(ext.getOrderSn());
            if (order == null) {
                continue;
            }
            String calendarStatus = resolveCalendarStatus(order);
            if (CharSequenceUtil.isNotBlank(status) && !"ALL".equalsIgnoreCase(status)
                    && !status.equalsIgnoreCase(calendarStatus)) {
                continue;
            }
            AppointmentWorkOrder workOrder = workOrderMap.get(ext.getOrderSn());
            AppointmentCalendarEntryVO entry = new AppointmentCalendarEntryVO();
            entry.setOrderSn(ext.getOrderSn());
            entry.setAppointmentSn(workOrder == null ? ext.getOrderSn() : workOrder.getWorkOrderSn());
            entry.setCustomerName(ext.getContactName());
            entry.setSlotTime(ext.getSlotStartTime() + "-" + ext.getSlotEndTime());
            entry.setServiceStatus(calendarStatus);
            if (workOrder != null && CharSequenceUtil.isNotBlank(workOrder.getStaffId())) {
                entry.setStaffName(staffNameMap.get(workOrder.getStaffId()));
            }
            entries.add(entry);
        }
        return entries;
    }

    @Override
    public Map<String, Object> getInStoreSettings(String storeId) {
        AppointmentStoreConfig config = loadStoreConfig(storeId, AppointmentServiceModeEnum.IN_STORE.name());
        Map<String, Object> form = new LinkedHashMap<>();
        form.put("userBooking", true);
        form.put("workOrder", config == null || !Boolean.FALSE.equals(config.getWorkOrderVisible()));
        form.put("endService", true);
        return form;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveInStoreSettings(String storeId, Map<String, Object> form) {
        AppointmentStoreConfigDTO dto = new AppointmentStoreConfigDTO();
        dto.setServiceMode(AppointmentServiceModeEnum.IN_STORE.name());
        dto.setWorkOrderVisible(readBoolean(form, "workOrder", true));
        saveStoreConfig(storeId, dto);
    }

    @Override
    public Map<String, Object> getOnSiteSettings(String storeId) {
        AppointmentStoreConfig config = loadStoreConfig(storeId, AppointmentServiceModeEnum.ON_SITE.name());
        JSONObject json = config == null || CharSequenceUtil.isBlank(config.getConfigJson())
                ? new JSONObject() : JSONUtil.parseObj(config.getConfigJson());
        Map<String, Object> form = new LinkedHashMap<>();
        form.put("userBooking", true);
        form.put("acceptOrder", config == null || !Boolean.FALSE.equals(config.getAcceptOrderEnabled()));
        form.put("onSiteCheckin", json.getBool("onSiteCheckin", true));
        form.put("serviceEvidence", json.getBool("serviceEvidence", false));
        form.put("endService", true);
        form.put("serviceAreaEnabled", json.getBool("serviceAreaEnabled", false));
        form.put("serviceAreaGeo", json.getStr("serviceAreaGeo", ""));
        return form;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveOnSiteSettings(String storeId, Map<String, Object> form) {
        JSONObject json = new JSONObject();
        json.set("onSiteCheckin", readBoolean(form, "onSiteCheckin", true));
        json.set("serviceEvidence", readBoolean(form, "serviceEvidence", false));
        boolean serviceAreaEnabled = readBoolean(form, "serviceAreaEnabled", false);
        String serviceAreaGeo = form == null ? "" : Objects.toString(form.get("serviceAreaGeo"), "");
        if (serviceAreaEnabled) {
            String normalized = AppointmentGeoFence.normalize(serviceAreaGeo);
            if (normalized == null) {
                throw new ServiceException(ResultCode.APPOINTMENT_SERVICE_AREA_INVALID);
            }
            serviceAreaGeo = normalized;
        } else if (CharSequenceUtil.isNotBlank(serviceAreaGeo)) {
            String normalized = AppointmentGeoFence.normalize(serviceAreaGeo);
            if (normalized != null) {
                serviceAreaGeo = normalized;
            }
        }
        json.set("serviceAreaEnabled", serviceAreaEnabled);
        json.set("serviceAreaGeo", serviceAreaGeo);

        AppointmentStoreConfigDTO dto = new AppointmentStoreConfigDTO();
        dto.setServiceMode(AppointmentServiceModeEnum.ON_SITE.name());
        dto.setAcceptOrderEnabled(readBoolean(form, "acceptOrder", true));
        dto.setConfigJson(json.toString());
        saveStoreConfig(storeId, dto);
    }

    private void ensureSlotStockForDate(GoodsSku sku, AppointmentGoodsExt ext, Date slotDate) {
        if (sku == null || slotDate == null) {
            return;
        }
        Date day = DateUtil.beginOfDay(slotDate);
        List<AppointmentSlotTemplate> templates = slotTemplateMapper.selectList(new LambdaQueryWrapper<AppointmentSlotTemplate>()
                .eq(AppointmentSlotTemplate::getSkuId, sku.getId())
                .eq(AppointmentSlotTemplate::getEnabled, true)
                .eq(AppointmentSlotTemplate::getDeleteFlag, false)
                .orderByAsc(AppointmentSlotTemplate::getSortOrder));
        Set<String> created = new HashSet<>();
        for (AppointmentSlotTemplate template : templates) {
            if (template == null || CharSequenceUtil.isBlank(template.getSlotStartTime())
                    || CharSequenceUtil.isBlank(template.getSlotEndTime())) {
                continue;
            }
            String slotKey = template.getSlotStartTime() + "-" + template.getSlotEndTime();
            if (!created.add(slotKey)) {
                continue;
            }
            long exists = slotStockMapper.selectCount(new LambdaQueryWrapper<AppointmentSlotStock>()
                    .eq(AppointmentSlotStock::getSkuId, sku.getId())
                    .eq(AppointmentSlotStock::getSlotDate, day)
                    .eq(AppointmentSlotStock::getSlotStartTime, template.getSlotStartTime())
                    .eq(AppointmentSlotStock::getSlotEndTime, template.getSlotEndTime()));
            if (exists > 0) {
                continue;
            }
            AppointmentSlotStock stock = new AppointmentSlotStock();
            stock.setId(SnowFlake.getIdStr());
            stock.setGoodsId(sku.getGoodsId());
            stock.setSkuId(sku.getId());
            stock.setStoreId(sku.getStoreId());
            stock.setSlotDate(day);
            stock.setSlotStartTime(template.getSlotStartTime());
            stock.setSlotEndTime(template.getSlotEndTime());
            stock.setCapacity(template.getCapacity());
            stock.setAvailableQty(template.getCapacity() == null || template.getCapacity() == 0
                    ? Integer.MAX_VALUE : template.getCapacity());
            stock.setVersion(0);
            stock.setDeleteFlag(false);
            try {
                slotStockMapper.insert(stock);
            } catch (DuplicateKeyException ignored) {
                // 同一日期被并发查询时，另一请求已写入该时段
            }
        }
    }

    private void fillSystemFormSnapshot(AppointmentGoodsExt ext, String storeId) {
        if (ext == null) {
            return;
        }
        if (CharSequenceUtil.isBlank(ext.getSystemFormId())) {
            ext.setSystemFormId(null);
            ext.setSystemFormSnapshot(null);
            return;
        }
        AppointmentSystemForm form = systemFormMapper.selectById(ext.getSystemFormId());
        if (form == null || Boolean.TRUE.equals(form.getDeleteFlag())
                || (CharSequenceUtil.isNotBlank(storeId) && !storeId.equals(form.getStoreId()))) {
            throw new ServiceException(ResultCode.APPOINTMENT_SYSTEM_FORM_NOT_FOUND);
        }
        ext.setSystemFormSnapshot(AppointmentFormAnswerSupport.snapshotSchema(form.getFormSchema()));
    }

    private void validateAndEnrichCheckout(TradeDTO tradeDTO, AppointmentCheckoutDTO checkout) {
        List<CartSkuVO> checked = tradeDTO.getCheckedSkuList();
        if (CollUtil.isEmpty(checked)) {
            checked = tradeDTO.getSkuList();
        }
        if (CollUtil.isEmpty(checked) || checked.get(0).getGoodsSku() == null) {
            throw new ServiceException(ResultCode.APPOINTMENT_SKU_NOT_FOUND);
        }
        CartSkuVO line = checked.get(0);
        String skuId = line.getGoodsSku().getId();
        if (CharSequenceUtil.isNotBlank(checkout.getSkuId()) && !skuId.equals(checkout.getSkuId())) {
            throw new ServiceException(ResultCode.APPOINTMENT_SKU_NOT_FOUND);
        }
        checkout.setSkuId(skuId);
        AppointmentSlotStock stock = slotStockMapper.selectById(checkout.getSlotStockId());
        if (stock == null || Boolean.TRUE.equals(stock.getDeleteFlag()) || !skuId.equals(stock.getSkuId())) {
            throw new ServiceException(ResultCode.APPOINTMENT_SLOT_NOT_FOUND);
        }
        if (!isSlotStartAfterNow(stock.getSlotDate(), stock.getSlotStartTime())) {
            throw new ServiceException(ResultCode.APPOINTMENT_SLOT_EXPIRED);
        }
        int quantity = line.getNum() == null ? 1 : line.getNum();
        if (stock.getAvailableQty() != null && stock.getAvailableQty() < quantity) {
            throw new ServiceException(ResultCode.APPOINTMENT_SLOT_INSUFFICIENT);
        }
        checkout.setSlotDate(stock.getSlotDate());
        checkout.setSlotStartTime(stock.getSlotStartTime());
        checkout.setSlotEndTime(stock.getSlotEndTime());
        if (CharSequenceUtil.isBlank(checkout.getServiceMode())) {
            throw new ServiceException(ResultCode.APPOINTMENT_CHECKOUT_CONTEXT_MISSING);
        }
        if (AppointmentServiceModeEnum.ON_SITE.name().equals(checkout.getServiceMode())
                && CharSequenceUtil.isBlank(checkout.getServiceAddressSnapshot())) {
            throw new ServiceException(ResultCode.APPOINTMENT_CHECKOUT_CONTEXT_MISSING);
        }
        AppointmentGoodsExt goodsExt = this.getOne(new LambdaQueryWrapper<AppointmentGoodsExt>()
                .eq(AppointmentGoodsExt::getGoodsId, line.getGoodsSku().getGoodsId())
                .eq(AppointmentGoodsExt::getDeleteFlag, false));
        if (!isBookableSlotDate(goodsExt, stock.getSlotDate())) {
            throw new ServiceException(ResultCode.APPOINTMENT_DATE_NOT_BOOKABLE);
        }
        if (goodsExt != null && CharSequenceUtil.isNotBlank(goodsExt.getSystemFormId())) {
            checkout.setFormAnswerJson(buildFormAnswerJson(checkout, goodsExt, quantity));
        }
        if (AppointmentServiceModeEnum.ON_SITE.name().equals(checkout.getServiceMode())) {
            String storeId = goodsExt == null ? null : goodsExt.getStoreId();
            if (CharSequenceUtil.isBlank(storeId) && line.getGoodsSku() != null) {
                storeId = line.getGoodsSku().getStoreId();
            }
            assertInsideServiceArea(storeId, checkout);
        }
    }

    private boolean isServiceAreaEnabled(String storeId) {
        return serviceAreaPolygons(storeId) != null;
    }

    private void assertInsideServiceArea(String storeId, AppointmentCheckoutDTO checkout) {
        List<List<double[]>> polygons = serviceAreaPolygons(storeId);
        if (polygons == null) {
            return;
        }
        Double lng = checkout.getServiceLongitude();
        Double lat = checkout.getServiceLatitude();
        if (lng == null || lat == null) {
            throw new ServiceException(ResultCode.APPOINTMENT_SERVICE_AREA_POINT_REQUIRED);
        }
        if (!AppointmentGeoFence.containsAny(lng, lat, polygons)) {
            throw new ServiceException(ResultCode.APPOINTMENT_SERVICE_AREA_OUTSIDE);
        }
    }

    private List<List<double[]>> serviceAreaPolygons(String storeId) {
        if (CharSequenceUtil.isBlank(storeId)) {
            return null;
        }
        AppointmentStoreConfig config = loadStoreConfig(storeId, AppointmentServiceModeEnum.ON_SITE.name());
        if (config == null || CharSequenceUtil.isBlank(config.getConfigJson())) {
            return null;
        }
        JSONObject json = JSONUtil.parseObj(config.getConfigJson());
        if (!json.getBool("serviceAreaEnabled", false)) {
            return null;
        }
        List<List<double[]>> polygons = AppointmentGeoFence.parsePolygons(json.getStr("serviceAreaGeo"));
        return polygons.isEmpty() ? null : polygons;
    }

    private AppointmentCheckoutDTO resolveCheckout(TradeDTO tradeDTO) {
        if (tradeDTO == null) {
            return null;
        }
        if (tradeDTO.getAppointmentCheckout() != null) {
            return tradeDTO.getAppointmentCheckout();
        }
        return AppointmentCheckoutContextHolder.get(tradeDTO.getMemberId());
    }

    private String buildFormAnswerJson(AppointmentCheckoutDTO checkout, AppointmentGoodsExt goodsExt, int quantity) {
        if (goodsExt == null || CharSequenceUtil.isBlank(goodsExt.getSystemFormId())) {
            return null;
        }
        String schema = goodsExt.getSystemFormSnapshot();
        if (CharSequenceUtil.isBlank(schema)) {
            AppointmentSystemForm form = systemFormMapper.selectById(goodsExt.getSystemFormId());
            schema = form == null ? null : form.getFormSchema();
        }
        return AppointmentFormAnswerSupport.normalize(
                checkout.getFormAnswerJson(),
                schema,
                goodsExt.getSystemFormId(),
                goodsExt.getFormSubmitMode(),
                quantity);
    }

    private String buildCancelPolicySnapshot(AppointmentGoodsExt goodsExt) {
        if (goodsExt == null) {
            return null;
        }
        JSONObject json = new JSONObject();
        json.set("cancelMode", goodsExt.getCancelMode());
        json.set("cancelHours", goodsExt.getCancelHours());
        return json.toString();
    }

    private AppointmentCheckoutDTO resolveCheckoutFromOrder(Order order) {
        AppointmentCheckoutDTO checkout = AppointmentCheckoutContextHolder.get(order.getMemberId());
        if (checkout != null) {
            return checkout;
        }
        AppointmentOrderExt existing = orderExtMapper.selectOne(new LambdaQueryWrapper<AppointmentOrderExt>()
                .eq(AppointmentOrderExt::getOrderSn, order.getSn()));
        if (existing == null) {
            return null;
        }
        checkout = new AppointmentCheckoutDTO();
        checkout.setSkuId(existing.getSkuId());
        checkout.setSlotStockId(existing.getSlotStockId());
        checkout.setServiceMode(existing.getServiceMode());
        checkout.setSlotDate(existing.getSlotDate());
        checkout.setSlotStartTime(existing.getSlotStartTime());
        checkout.setSlotEndTime(existing.getSlotEndTime());
        checkout.setContactName(existing.getContactName());
        checkout.setContactPhone(existing.getContactPhone());
        checkout.setServiceAddressSnapshot(existing.getServiceAddressSnapshot());
        checkout.setFormAnswerJson(existing.getFormAnswerJson());
        return checkout;
    }

    private void createWorkOrderAndCodes(Order order, AppointmentOrderExt orderExt, int quantity) {
        AppointmentStoreConfig storeConfig = storeConfigMapper.selectOne(new LambdaQueryWrapper<AppointmentStoreConfig>()
                .eq(AppointmentStoreConfig::getStoreId, order.getStoreId())
                .eq(AppointmentStoreConfig::getServiceMode, orderExt.getServiceMode())
                .eq(AppointmentStoreConfig::getDeleteFlag, false));
        String initialStatus;
        if (AppointmentServiceModeEnum.ON_SITE.name().equals(orderExt.getServiceMode())
                && storeConfig != null && Boolean.TRUE.equals(storeConfig.getAcceptOrderEnabled())) {
            initialStatus = AppointmentWorkOrderStatusEnum.PENDING_CLAIM.name();
        } else {
            initialStatus = AppointmentWorkOrderStatusEnum.WAIT_VERIFY.name();
        }
        AppointmentWorkOrder workOrder = new AppointmentWorkOrder();
        workOrder.setId(SnowFlake.getIdStr());
        workOrder.setWorkOrderSn(AppointmentSnUtil.workOrderSn());
        workOrder.setOrderSn(order.getSn());
        workOrder.setMemberId(order.getMemberId());
        workOrder.setStoreId(order.getStoreId());
        workOrder.setServiceMode(orderExt.getServiceMode());
        workOrder.setServiceStatus(initialStatus);
        workOrder.setAppointmentTime(buildAppointmentDateTime(orderExt));
        workOrder.setBookerName(orderExt.getContactName());
        workOrder.setBookerPhone(orderExt.getContactPhone());
        workOrder.setAppointmentAddress(orderExt.getServiceAddressSnapshot());
        workOrder.setDeleteFlag(false);
        workOrderMapper.insert(workOrder);

        List<OrderItem> items = orderItemService.getByOrderSn(order.getSn());
        String orderItemSn = CollUtil.isEmpty(items) ? null : items.get(0).getSn();
        Set<String> usedCodes = new HashSet<>();
        for (int i = 1; i <= quantity; i++) {
            AppointmentVerificationCode code = new AppointmentVerificationCode();
            code.setId(SnowFlake.getIdStr());
            code.setOrderSn(order.getSn());
            code.setOrderItemSn(orderItemSn);
            code.setStoreId(order.getStoreId());
            code.setMemberId(order.getMemberId());
            code.setSeqNo(i);
            code.setStatus(AppointmentVerificationCodeStatusEnum.UNUSED.name());
            code.setCode(generateUniqueCode(order.getStoreId(), usedCodes));
            code.setDeleteFlag(false);
            verificationCodeMapper.insert(code);
        }
    }

    /**
     * 可约日 = 今天起 bookingDateRangeDays 天，自定义模式再与日期区间、星期取交集。
     * 商家星期：周一=1 … 周日=7。
     */
    private boolean isBookableSlotDate(AppointmentGoodsExt ext, Date slotDate) {
        if (slotDate == null) {
            return false;
        }
        LocalDate date = toLocalDate(slotDate);
        LocalDate today = LocalDate.now();
        int rangeDays = 30;
        if (ext != null && ext.getBookingDateRangeDays() != null && ext.getBookingDateRangeDays() > 0) {
            rangeDays = ext.getBookingDateRangeDays();
        }
        if (date.isBefore(today) || date.isAfter(today.plusDays(rangeDays - 1L))) {
            return false;
        }
        if (ext == null || !"CUSTOM".equalsIgnoreCase(ext.getBookableDateMode())) {
            return true;
        }
        if (ext.getCustomDateStart() != null && date.isBefore(toLocalDate(ext.getCustomDateStart()))) {
            return false;
        }
        if (ext.getCustomDateEnd() != null && date.isAfter(toLocalDate(ext.getCustomDateEnd()))) {
            return false;
        }
        Set<Integer> weekdays = parseCustomWeekdays(ext.getCustomWeekdays());
        return weekdays.isEmpty() || weekdays.contains(date.getDayOfWeek().getValue());
    }

    private Set<Integer> parseCustomWeekdays(String raw) {
        Set<Integer> days = new HashSet<>();
        if (CharSequenceUtil.isBlank(raw)) {
            return days;
        }
        for (String part : raw.split(",")) {
            if (CharSequenceUtil.isBlank(part)) {
                continue;
            }
            try {
                int day = Integer.parseInt(part.trim());
                if (day >= 1 && day <= 7) {
                    days.add(day);
                }
            } catch (NumberFormatException ignored) {
                // 忽略无法解析的星期
            }
        }
        return days;
    }

    private LocalDate toLocalDate(Date date) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        return LocalDate.of(
                calendar.get(Calendar.YEAR),
                calendar.get(Calendar.MONTH) + 1,
                calendar.get(Calendar.DAY_OF_MONTH));
    }

    private boolean isSlotStartAfterNow(Date slotDate, String slotStartTime) {
        if (slotDate == null || CharSequenceUtil.isBlank(slotStartTime)) {
            return false;
        }
        Date start = DateUtil.parse(DateUtil.formatDate(slotDate) + " " + slotStartTime + ":00");
        return start != null && start.after(new Date());
    }

    private Date buildAppointmentDateTime(AppointmentOrderExt orderExt) {
        String datePart = DateUtil.formatDate(orderExt.getSlotDate());
        return DateUtil.parse(datePart + " " + orderExt.getSlotStartTime() + ":00");
    }

    private String generateUniqueCode(String storeId, Set<String> usedCodes) {
        for (int attempt = 0; attempt < 20; attempt++) {
            String code = AppointmentSnUtil.verificationCode(storeId);
            if (usedCodes.add(code) && verificationCodeMapper.selectCount(new LambdaQueryWrapper<AppointmentVerificationCode>()
                    .eq(AppointmentVerificationCode::getStoreId, storeId)
                    .eq(AppointmentVerificationCode::getCode, code)) == 0) {
                return code;
            }
        }
        throw new ServiceException(ResultCode.APPOINTMENT_CODE_GENERATE_FAILED);
    }

    private void transitionOrderToTake(String orderSn) {
        boolean updated = orderService.update(new LambdaUpdateWrapper<Order>()
                .eq(Order::getSn, orderSn)
                .eq(Order::getOrderType, OrderTypeEnum.APPOINTMENT.name())
                .eq(Order::getOrderStatus, OrderStatusEnum.PAID.name())
                .set(Order::getOrderStatus, OrderStatusEnum.TAKE.name()));
        if (updated) {
            OrderMessage orderMessage = new OrderMessage();
            orderMessage.setOrderSn(orderSn);
            orderMessage.setNewStatus(OrderStatusEnum.TAKE);
            orderService.sendUpdateStatusMessage(orderMessage);
        }
    }

    private void failFulfillment(String orderSn, String slotStockId, int quantity) {
        if (CharSequenceUtil.isNotBlank(slotStockId) && quantity > 0) {
            restoreSlotStock(slotStockId, quantity);
        }
        orderService.systemCancel(orderSn, FULFILL_FAIL_CANCEL_REASON, true);
    }

    private AppointmentVerificationCode loadVerificationCode(String storeId, String code) {
        if (CharSequenceUtil.isBlank(code)) {
            throw new ServiceException(ResultCode.APPOINTMENT_CODE_NOT_FOUND);
        }
        AppointmentVerificationCode verificationCode = verificationCodeMapper.selectOne(
                new LambdaQueryWrapper<AppointmentVerificationCode>()
                        .eq(AppointmentVerificationCode::getStoreId, storeId)
                        .eq(AppointmentVerificationCode::getCode, code)
                        .eq(AppointmentVerificationCode::getDeleteFlag, false));
        if (verificationCode == null) {
            throw new ServiceException(ResultCode.APPOINTMENT_CODE_NOT_FOUND);
        }
        return verificationCode;
    }

    private AppointmentServiceStaff validateEnabledStaff(String storeId, String staffId) {
        if (CharSequenceUtil.isBlank(staffId)) {
            throw new ServiceException(ResultCode.APPOINTMENT_STAFF_REQUIRED);
        }
        AppointmentServiceStaff staff = serviceStaffMapper.selectById(staffId);
        if (staff == null || !storeId.equals(staff.getStoreId()) || Boolean.TRUE.equals(staff.getDeleteFlag())) {
            throw new ServiceException(ResultCode.APPOINTMENT_STAFF_NOT_FOUND);
        }
        if (!"ENABLED".equalsIgnoreCase(staff.getStatus())) {
            throw new ServiceException(ResultCode.APPOINTMENT_STAFF_DISABLED);
        }
        return staff;
    }

    private void validateAppointmentOrderForVerify(Order order, String storeId) {
        if (order == null || !OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        if (!storeId.equals(order.getStoreId())) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        if (!OrderStatusEnum.TAKE.name().equals(order.getOrderStatus())) {
            throw new ServiceException(ResultCode.APPOINTMENT_ORDER_NOT_TAKE);
        }
    }

    private void enforceSameStaffForOrder(String orderSn, String staffId) {
        List<AppointmentVerificationCode> usedCodes = verificationCodeMapper.selectList(
                new LambdaQueryWrapper<AppointmentVerificationCode>()
                        .eq(AppointmentVerificationCode::getOrderSn, orderSn)
                        .eq(AppointmentVerificationCode::getStatus, AppointmentVerificationCodeStatusEnum.USED.name())
                        .eq(AppointmentVerificationCode::getDeleteFlag, false));
        for (AppointmentVerificationCode usedCode : usedCodes) {
            if (CharSequenceUtil.isNotBlank(usedCode.getStaffId()) && !staffId.equals(usedCode.getStaffId())) {
                throw new ServiceException(ResultCode.APPOINTMENT_CODE_STAFF_MISMATCH);
            }
        }
    }

    private List<AppointmentVerificationCode> listOrderVerificationCodes(String orderSn) {
        return verificationCodeMapper.selectList(new LambdaQueryWrapper<AppointmentVerificationCode>()
                .eq(AppointmentVerificationCode::getOrderSn, orderSn)
                .eq(AppointmentVerificationCode::getDeleteFlag, false)
                .orderByAsc(AppointmentVerificationCode::getSeqNo));
    }

    private int countUsedCodes(List<AppointmentVerificationCode> orderCodes) {
        return (int) orderCodes.stream()
                .filter(code -> AppointmentVerificationCodeStatusEnum.USED.name().equals(code.getStatus()))
                .count();
    }

    private AppointmentOrderExt loadOrderExt(String orderSn) {
        AppointmentOrderExt orderExt = orderExtMapper.selectOne(new LambdaQueryWrapper<AppointmentOrderExt>()
                .eq(AppointmentOrderExt::getOrderSn, orderSn)
                .eq(AppointmentOrderExt::getDeleteFlag, false));
        if (orderExt == null) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        return orderExt;
    }

    private AppointmentWorkOrder loadWorkOrderByOrderSn(String orderSn) {
        AppointmentWorkOrder workOrder = workOrderMapper.selectOne(new LambdaQueryWrapper<AppointmentWorkOrder>()
                .eq(AppointmentWorkOrder::getOrderSn, orderSn)
                .eq(AppointmentWorkOrder::getDeleteFlag, false));
        if (workOrder == null) {
            throw new ServiceException(ResultCode.APPOINTMENT_WORK_ORDER_NOT_FOUND);
        }
        return workOrder;
    }

    private AppointmentWorkOrder loadStoreWorkOrder(String storeId, String workOrderId) {
        AppointmentWorkOrder workOrder = workOrderMapper.selectById(workOrderId);
        if (workOrder == null || !storeId.equals(workOrder.getStoreId()) || Boolean.TRUE.equals(workOrder.getDeleteFlag())) {
            throw new ServiceException(ResultCode.APPOINTMENT_WORK_ORDER_NOT_FOUND);
        }
        return workOrder;
    }

    private AppointmentVerificationOrderVO buildVerificationOrderVO(String orderSn, String storeId) {
        Order order = orderMapper.selectOne(new LambdaQueryWrapper<Order>()
                .eq(Order::getSn, orderSn)
                .eq(Order::getStoreId, storeId));
        if (order == null || !OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        AppointmentOrderExt orderExt = loadOrderExt(orderSn);
        AppointmentWorkOrder workOrder = workOrderMapper.selectOne(new LambdaQueryWrapper<AppointmentWorkOrder>()
                .eq(AppointmentWorkOrder::getOrderSn, orderSn)
                .eq(AppointmentWorkOrder::getDeleteFlag, false));
        List<AppointmentVerificationCode> orderCodes = listOrderVerificationCodes(orderSn);
        Map<String, String> staffNameMap = loadStaffNameIds(orderCodes.stream()
                .map(AppointmentVerificationCode::getStaffId)
                .filter(CharSequenceUtil::isNotBlank)
                .collect(Collectors.toSet()));

        AppointmentVerificationOrderVO vo = new AppointmentVerificationOrderVO();
        vo.setOrderSn(order.getSn());
        vo.setOrderStatus(order.getOrderStatus());
        vo.setServiceMode(orderExt.getServiceMode());
        vo.setSlotDate(orderExt.getSlotDate());
        vo.setSlotStartTime(orderExt.getSlotStartTime());
        vo.setSlotEndTime(orderExt.getSlotEndTime());
        vo.setFormAnswerJson(orderExt.getFormAnswerJson());
        vo.setWorkOrderVisible(Boolean.TRUE.equals(orderExt.getWorkOrderVisibleSnapshot()));
        vo.setTotalCount(orderCodes.size());
        vo.setVerifiedCount(countUsedCodes(orderCodes));
        if (workOrder != null) {
            vo.setWorkOrderId(workOrder.getId());
            vo.setWorkOrderSn(workOrder.getWorkOrderSn());
            vo.setWorkOrderStatus(workOrder.getServiceStatus());
        }
        vo.setContactName(orderExt.getContactName());
        vo.setContactPhone(orderExt.getContactPhone());
        vo.setCodes(orderCodes.stream().map(code -> {
            AppointmentVerificationCodeVO codeVO = BeanUtil.copyProperties(code, AppointmentVerificationCodeVO.class);
            if (CharSequenceUtil.isNotBlank(code.getStaffId())) {
                codeVO.setStaffName(staffNameMap.get(code.getStaffId()));
            }
            return codeVO;
        }).collect(Collectors.toList()));
        return vo;
    }

    private Map<String, String> loadStaffNameMap(Collection<AppointmentWorkOrder> records) {
        Set<String> staffIds = records.stream()
                .map(AppointmentWorkOrder::getStaffId)
                .filter(CharSequenceUtil::isNotBlank)
                .collect(Collectors.toSet());
        return loadStaffNameIds(staffIds);
    }

    private Map<String, String> loadStaffNameIds(Set<String> staffIds) {
        if (CollUtil.isEmpty(staffIds)) {
            return Collections.emptyMap();
        }
        return serviceStaffMapper.selectBatchIds(staffIds).stream()
                .collect(Collectors.toMap(AppointmentServiceStaff::getId, AppointmentServiceStaff::getStaffName, (a, b) -> a));
    }

    private AppointmentStoreConfig loadStoreConfig(String storeId, String serviceMode) {
        return storeConfigMapper.selectOne(new LambdaQueryWrapper<AppointmentStoreConfig>()
                .eq(AppointmentStoreConfig::getStoreId, storeId)
                .eq(AppointmentStoreConfig::getServiceMode, serviceMode)
                .eq(AppointmentStoreConfig::getDeleteFlag, false));
    }

    private boolean readBoolean(Map<String, Object> form, String key, boolean defaultValue) {
        if (form == null || !form.containsKey(key) || form.get(key) == null) {
            return defaultValue;
        }
        Object value = form.get(key);
        if (value instanceof Boolean) {
            return (Boolean) value;
        }
        return Boolean.parseBoolean(String.valueOf(value));
    }

    private String resolveCalendarStatus(Order order) {
        if (OrderStatusEnum.CANCELLED.name().equals(order.getOrderStatus())) {
            return "CANCELED";
        }
        if (OrderStatusEnum.COMPLETED.name().equals(order.getOrderStatus())) {
            return "ARRIVED";
        }
        return "WAITING_ARRIVAL";
    }

    // ===================== batch 6-11 lifecycle =====================

    @Override
    public boolean isAppointmentOrder(Order order) {
        return order != null && OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType());
    }

    @Override
    public boolean isServiceComplete(String orderSn) {
        AppointmentWorkOrder workOrder = workOrderMapper.selectOne(new LambdaQueryWrapper<AppointmentWorkOrder>()
                .eq(AppointmentWorkOrder::getOrderSn, orderSn)
                .eq(AppointmentWorkOrder::getDeleteFlag, false));
        if (workOrder != null && CharSequenceUtil.equalsAny(workOrder.getServiceStatus(),
                AppointmentWorkOrderStatusEnum.SERVICE_ENDED.name(),
                AppointmentWorkOrderStatusEnum.NO_SHOW.name())) {
            return true;
        }
        long total = verificationCodeMapper.selectCount(new LambdaQueryWrapper<AppointmentVerificationCode>()
                .eq(AppointmentVerificationCode::getOrderSn, orderSn)
                .eq(AppointmentVerificationCode::getDeleteFlag, false));
        if (total == 0) {
            return false;
        }
        long unused = verificationCodeMapper.selectCount(new LambdaQueryWrapper<AppointmentVerificationCode>()
                .eq(AppointmentVerificationCode::getOrderSn, orderSn)
                .eq(AppointmentVerificationCode::getStatus, AppointmentVerificationCodeStatusEnum.UNUSED.name())
                .eq(AppointmentVerificationCode::getDeleteFlag, false));
        return unused == 0;
    }

    @Override
    public boolean canApplyBuyerAfterSale(String orderSn) {
        Order order = orderService.getBySn(orderSn);
        if (!isAppointmentOrder(order) || !PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
            return false;
        }
        if (OrderStatusEnum.CANCELLED.name().equals(order.getOrderStatus())) {
            return false;
        }
        return !isServiceComplete(orderSn) && getRemainingRefundableAmount(orderSn) > 0;
    }

    @Override
    public double getRemainingRefundableAmount(String orderSn) {
        Order order = orderService.getBySn(orderSn);
        if (order == null) {
            return 0D;
        }
        AppointmentOrderExt ext = getOrderExt(orderSn);
        double refunded = ext == null || ext.getRefundedAmount() == null ? 0D : ext.getRefundedAmount();
        return Math.max(0D, CurrencyUtil.sub(order.getFlowPrice(), refunded));
    }

    @Override
    public void validateBuyerAfterSaleAmount(String orderSn, double applyAmount) {
        if (applyAmount <= 0) {
            throw new ServiceException(ResultCode.APPOINTMENT_REFUND_AMOUNT_INVALID);
        }
        if (!canApplyBuyerAfterSale(orderSn)) {
            throw new ServiceException(ResultCode.APPOINTMENT_AFTER_SALE_SERVICE_COMPLETE);
        }
        if (applyAmount > getRemainingRefundableAmount(orderSn)) {
            throw new ServiceException(ResultCode.APPOINTMENT_REFUND_AMOUNT_INVALID);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void onRefundSuccess(String orderSn, double refundAmount) {
        if (refundAmount <= 0) {
            return;
        }
        Order order = orderMapper.selectForUpdateBySn(orderSn);
        if (!isAppointmentOrder(order)) {
            return;
        }
        AppointmentOrderExt ext = getOrderExt(orderSn);
        if (ext == null) {
            return;
        }
        double refunded = ext.getRefundedAmount() == null ? 0D : ext.getRefundedAmount();
        double newRefunded = CurrencyUtil.add(refunded, refundAmount);
        ext.setRefundedAmount(newRefunded);
        orderExtMapper.updateById(ext);
        if (newRefunded < order.getFlowPrice() - 0.001D) {
            return;
        }
        closeAppointmentAfterFullRefund(order, ext);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void platformRefund(String orderSn, AppointmentPlatformRefundDTO dto) {
        Order order = requirePaidAppointmentOrder(orderSn);
        boolean served = isServiceComplete(orderSn);
        if (AppointmentPlatformRefundTypeEnum.SERVED.name().equals(dto.getRefundType()) && !served) {
            throw new ServiceException(ResultCode.APPOINTMENT_PLATFORM_REFUND_TYPE_MISMATCH);
        }
        if (AppointmentPlatformRefundTypeEnum.UNSERVED.name().equals(dto.getRefundType()) && served) {
            throw new ServiceException(ResultCode.APPOINTMENT_PLATFORM_REFUND_TYPE_MISMATCH);
        }
        validateRefundAmount(orderSn, dto.getRefundAmount());
        refundChannel(order, dto.getRefundAmount(), dto.getReason());
        onRefundSuccess(orderSn, dto.getRefundAmount());
        appointmentNoticeSupport.notifyServiceRefunded(order.getMemberId(), getGoodsName(orderSn), orderSn);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void noShowRefund(String orderSn, AppointmentNoShowRefundDTO dto) {
        Order order = requirePaidAppointmentOrder(orderSn);
        AppointmentOrderExt ext = requireOrderExt(orderSn);
        if (!AppointmentServiceModeEnum.ON_SITE.name().equals(ext.getServiceMode())) {
            throw new ServiceException(ResultCode.APPOINTMENT_NO_SHOW_ON_SITE_ONLY);
        }
        if (isServiceComplete(orderSn)) {
            throw new ServiceException(ResultCode.APPOINTMENT_NO_SHOW_ALREADY_VERIFIED);
        }
        validateRefundAmount(orderSn, dto.getRefundAmount());
        refundChannel(order, dto.getRefundAmount(), dto.getReason());
        onRefundSuccess(orderSn, dto.getRefundAmount());
        appointmentNoticeSupport.notifyServiceRefunded(order.getMemberId(), getGoodsName(orderSn), orderSn);
    }

    @Override
    public IPage<AppointmentSystemForm> pageSystemForms(String storeId, int pageNumber, int pageSize,
                                                        String formName, String status) {
        Page<AppointmentSystemForm> page = new Page<>(pageNumber, pageSize);
        return systemFormMapper.selectPage(page, new LambdaQueryWrapper<AppointmentSystemForm>()
                .eq(AppointmentSystemForm::getStoreId, storeId)
                .eq(AppointmentSystemForm::getDeleteFlag, false)
                .like(CharSequenceUtil.isNotBlank(formName), AppointmentSystemForm::getFormName, formName)
                .eq(CharSequenceUtil.isNotBlank(status), AppointmentSystemForm::getStatus, status)
                .orderByDesc(AppointmentSystemForm::getUpdateTime));
    }

    @Override
    public AppointmentSystemForm getSystemForm(String storeId, String formId) {
        AppointmentSystemForm form = systemFormMapper.selectById(formId);
        if (form == null || !storeId.equals(form.getStoreId()) || Boolean.TRUE.equals(form.getDeleteFlag())) {
            throw new ServiceException(ResultCode.APPOINTMENT_SYSTEM_FORM_NOT_FOUND);
        }
        return form;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public AppointmentSystemForm saveSystemForm(String storeId, AppointmentSystemFormDTO dto) {
        AppointmentSystemForm form;
        if (CharSequenceUtil.isNotBlank(dto.getId())) {
            form = systemFormMapper.selectById(dto.getId());
            if (form == null || !storeId.equals(form.getStoreId())) {
                throw new ServiceException(ResultCode.APPOINTMENT_SYSTEM_FORM_NOT_FOUND);
            }
            form.setFormName(dto.getFormName());
            form.setFormSchema(AppointmentFormAnswerSupport.snapshotSchema(dto.getFormSchema()));
            if (CharSequenceUtil.isNotBlank(dto.getStatus())) {
                form.setStatus(dto.getStatus());
            }
            systemFormMapper.updateById(form);
        } else {
            form = new AppointmentSystemForm();
            form.setId(SnowFlake.getIdStr());
            form.setStoreId(storeId);
            form.setFormName(dto.getFormName());
            form.setFormSchema(AppointmentFormAnswerSupport.snapshotSchema(dto.getFormSchema()));
            form.setStatus(CharSequenceUtil.isBlank(dto.getStatus()) ? "ENABLED" : dto.getStatus());
            form.setDeleteFlag(false);
            systemFormMapper.insert(form);
        }
        return form;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteSystemForm(String storeId, String formId) {
        AppointmentSystemForm form = systemFormMapper.selectById(formId);
        if (form == null || !storeId.equals(form.getStoreId())) {
            throw new ServiceException(ResultCode.APPOINTMENT_SYSTEM_FORM_NOT_FOUND);
        }
        form.setDeleteFlag(true);
        systemFormMapper.updateById(form);
    }

    @Override
    public List<AppointmentSystemForm> listEnabledSystemForms(String storeId) {
        return systemFormMapper.selectList(new LambdaQueryWrapper<AppointmentSystemForm>()
                .eq(AppointmentSystemForm::getStoreId, storeId)
                .eq(AppointmentSystemForm::getStatus, "ENABLED")
                .eq(AppointmentSystemForm::getDeleteFlag, false)
                .orderByDesc(AppointmentSystemForm::getCreateTime));
    }

    @Override
    public AppointmentOrderSetting getAppointmentOrderSetting() {
        Setting setting = settingService.get(SettingEnum.APPOINTMENT_ORDER_SETTING.name());
        if (setting == null || CharSequenceUtil.isBlank(setting.getSettingValue())) {
            return new AppointmentOrderSetting();
        }
        return JSONUtil.toBean(setting.getSettingValue(), AppointmentOrderSetting.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveAppointmentOrderSetting(AppointmentOrderSetting setting) {
        if (setting == null) {
            setting = new AppointmentOrderSetting();
        }
        Setting entity = settingService.get(SettingEnum.APPOINTMENT_ORDER_SETTING.name());
        if (entity == null) {
            entity = new Setting();
            entity.setId(SettingEnum.APPOINTMENT_ORDER_SETTING.name());
        }
        entity.setSettingValue(JSONUtil.toJsonStr(setting));
        settingService.saveUpdate(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void processNoShowAutoComplete() {
        AppointmentOrderSetting cfg = getAppointmentOrderSetting();
        if (cfg.getInStoreNoShowAutoCompleteHours() == null || cfg.getInStoreNoShowAutoCompleteHours() <= 0) {
            return;
        }
        List<AppointmentWorkOrder> candidates = workOrderMapper.selectList(new LambdaQueryWrapper<AppointmentWorkOrder>()
                .in(AppointmentWorkOrder::getServiceStatus,
                        AppointmentWorkOrderStatusEnum.WAIT_VERIFY.name(),
                        AppointmentWorkOrderStatusEnum.PENDING_CLAIM.name(),
                        AppointmentWorkOrderStatusEnum.CLAIMED.name())
                .eq(AppointmentWorkOrder::getDeleteFlag, false));
        Date now = new Date();
        for (AppointmentWorkOrder workOrder : candidates) {
            AppointmentOrderExt ext = getOrderExt(workOrder.getOrderSn());
            if (ext == null || isServiceComplete(workOrder.getOrderSn())) {
                continue;
            }
            Date slotEnd = DateUtil.parse(DateUtil.formatDate(ext.getSlotDate()) + " " + ext.getSlotEndTime() + ":00");
            Date deadline = DateUtil.offsetHour(slotEnd, cfg.getInStoreNoShowAutoCompleteHours());
            if (now.before(deadline)) {
                continue;
            }
            completeNoShow(workOrder);
        }
    }

    @Override
    public void processServiceStartRemind() {
        AppointmentOrderSetting cfg = getAppointmentOrderSetting();
        if (cfg.getServiceStartRemindMemberHours() == null || cfg.getServiceStartRemindMemberHours() <= 0) {
            return;
        }
        List<AppointmentOrderExt> exts = orderExtMapper.selectList(new LambdaQueryWrapper<AppointmentOrderExt>()
                .eq(AppointmentOrderExt::getDeleteFlag, false));
        Date now = new Date();
        for (AppointmentOrderExt ext : exts) {
            if (isServiceComplete(ext.getOrderSn())) {
                continue;
            }
            Order order = orderService.getBySn(ext.getOrderSn());
            if (order == null || !PayStatusEnum.PAID.name().equals(order.getPayStatus())
                    || OrderStatusEnum.CANCELLED.name().equals(order.getOrderStatus())) {
                continue;
            }
            Date start = DateUtil.parse(DateUtil.formatDate(ext.getSlotDate()) + " " + ext.getSlotStartTime() + ":00");
            if (!start.after(now)) {
                continue;
            }
            long hours = DateUtil.between(now, start, cn.hutool.core.date.DateUnit.HOUR);
            if (hours > cfg.getServiceStartRemindMemberHours() || hours <= 0) {
                continue;
            }
            appointmentNoticeSupport.notifyMember(order.getMemberId(),
                    NoticeMessageNodeEnum.APPOINTMENT_SERVICE_START_REMIND_MEMBER,
                    getGoodsName(ext.getOrderSn()), ext.getOrderSn());
        }
    }

    @Override
    public void notifyBookingSuccess(String orderSn) {
        Order order = orderService.getBySn(orderSn);
        if (order == null) {
            return;
        }
        appointmentNoticeSupport.notifyMember(order.getMemberId(),
                NoticeMessageNodeEnum.APPOINTMENT_BOOKING_SUCCESS_MEMBER,
                getGoodsName(orderSn), orderSn);
    }

    private void closeAppointmentAfterFullRefund(Order order, AppointmentOrderExt ext) {
        int quantity = ext.getQuantity() == null ? 1 : ext.getQuantity();
        restoreSlotStock(ext.getSlotStockId(), quantity);
        voidVerificationCodes(order.getSn());
        workOrderMapper.update(null, new LambdaUpdateWrapper<AppointmentWorkOrder>()
                .eq(AppointmentWorkOrder::getOrderSn, order.getSn())
                .set(AppointmentWorkOrder::getServiceStatus, AppointmentWorkOrderStatusEnum.REFUNDED.name()));
        order.setOrderStatus(OrderStatusEnum.CANCELLED.name());
        order.setCancelReason("预约订单退款完结");
        orderMapper.updateById(order);
        orderItemService.update(new LambdaUpdateWrapper<OrderItem>()
                .eq(OrderItem::getOrderSn, order.getSn())
                .set(OrderItem::getIsRefund, RefundStatusEnum.ALL_REFUND.name()));
    }

    private void voidVerificationCodes(String orderSn) {
        verificationCodeMapper.update(null, new LambdaUpdateWrapper<AppointmentVerificationCode>()
                .eq(AppointmentVerificationCode::getOrderSn, orderSn)
                .eq(AppointmentVerificationCode::getStatus, AppointmentVerificationCodeStatusEnum.UNUSED.name())
                .set(AppointmentVerificationCode::getStatus, AppointmentVerificationCodeStatusEnum.VOIDED.name()));
    }

    private void completeNoShow(AppointmentWorkOrder workOrder) {
        workOrder.setServiceStatus(AppointmentWorkOrderStatusEnum.NO_SHOW.name());
        workOrderMapper.updateById(workOrder);
        voidVerificationCodes(workOrder.getOrderSn());
        Order order = orderService.getBySn(workOrder.getOrderSn());
        if (order != null && !OrderStatusEnum.COMPLETED.name().equals(order.getOrderStatus())) {
            orderService.systemComplete(workOrder.getOrderSn());
        }
    }

    private Order requirePaidAppointmentOrder(String orderSn) {
        Order order = orderService.getBySn(orderSn);
        if (!isAppointmentOrder(order)) {
            throw new ServiceException(ResultCode.APPOINTMENT_ORDER_NOT_FOUND);
        }
        if (!PayStatusEnum.PAID.name().equals(order.getPayStatus())) {
            throw new ServiceException(ResultCode.APPOINTMENT_AFTER_SALE_UNPAID);
        }
        return order;
    }

    private AppointmentOrderExt requireOrderExt(String orderSn) {
        AppointmentOrderExt ext = getOrderExt(orderSn);
        if (ext == null) {
            throw new ServiceException(ResultCode.APPOINTMENT_ORDER_NOT_FOUND);
        }
        return ext;
    }

    private AppointmentOrderExt getOrderExt(String orderSn) {
        return orderExtMapper.selectOne(new LambdaQueryWrapper<AppointmentOrderExt>()
                .eq(AppointmentOrderExt::getOrderSn, orderSn)
                .eq(AppointmentOrderExt::getDeleteFlag, false));
    }

    private void validateRefundAmount(String orderSn, double amount) {
        if (amount <= 0 || amount > getRemainingRefundableAmount(orderSn)) {
            throw new ServiceException(ResultCode.APPOINTMENT_REFUND_AMOUNT_INVALID);
        }
    }

    private void refundChannel(Order order, double amount, String reason) {
        RefundLog refundLog = RefundLog.builder()
                .isRefund(false)
                .totalAmount(amount)
                .payPrice(amount)
                .memberId(order.getMemberId())
                .paymentName(order.getPaymentMethod())
                .paymentReceivableNo(order.getReceivableNo())
                .outOrderNo("AP" + SnowFlake.getIdStr())
                .orderSn(order.getSn())
                .refundReason(reason)
                .build();
        PaymentMethodEnum paymentMethodEnum = PaymentMethodEnum.paymentNameOf(order.getPaymentMethod());
        Payment payment = (Payment) cn.lili.common.utils.SpringContextUtil.getBean(paymentMethodEnum.getPlugin());
        payment.refund(refundLog);
    }

    private String getGoodsName(String orderSn) {
        List<OrderItem> items = orderItemService.getByOrderSn(orderSn);
        return CollUtil.isEmpty(items) ? "" : items.get(0).getGoodsName();
    }
}
