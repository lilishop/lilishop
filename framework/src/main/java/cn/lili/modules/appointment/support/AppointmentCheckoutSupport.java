package cn.lili.modules.appointment.support;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.appointment.entity.dos.*;
import cn.lili.modules.appointment.entity.dto.AppointmentCheckoutDTO;
import cn.lili.modules.appointment.entity.enums.AppointmentServiceModeEnum;
import cn.lili.modules.appointment.entity.enums.AppointmentVerificationCodeStatusEnum;
import cn.lili.modules.appointment.entity.enums.AppointmentWorkOrderStatusEnum;
import cn.lili.modules.appointment.mapper.*;
import cn.lili.modules.appointment.util.AppointmentGeoFence;
import cn.lili.modules.appointment.util.AppointmentSnUtil;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.vo.OrderVO;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * 预约下单校验、上下文绑定与支付后履约业务支持组件
 * <p>
 * 负责加车/结算上下文校验、电子围栏判定、预约扩展信息持久化以及支付成功后的工单与核销码生成。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class AppointmentCheckoutSupport {

    private static final String FULFILL_FAIL_CANCEL_REASON = "预约时段库存不足，支付失败已退款";

    @Autowired
    private AppointmentSlotStockMapper slotStockMapper;

    @Autowired
    private AppointmentOrderExtMapper orderExtMapper;

    @Autowired
    private AppointmentWorkOrderMapper workOrderMapper;

    @Autowired
    private AppointmentVerificationCodeMapper verificationCodeMapper;

    @Autowired
    private AppointmentStoreConfigMapper storeConfigMapper;

    @Autowired
    private AppointmentGoodsExtMapper goodsExtMapper;

    @Autowired
    private AppointmentSystemFormMapper systemFormMapper;

    @Autowired
    private OrderMapper orderMapper;

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    private OrderService orderService;

    @Autowired
    private AppointmentSlotStockSupport appointmentSlotStockSupport;

    @Autowired
    private AppointmentVerificationSupport appointmentVerificationSupport;

    @Autowired
    private AppointmentConfigSupport appointmentConfigSupport;

    @Autowired
    private AppointmentAfterSaleSupport appointmentAfterSaleSupport;

    /**
     * 校验并填充结算上下文（预约时段有效性、余量及服务区域电子围栏）
     *
     * @param tradeDTO 交易DTO
     * @param checkout 预约结算上下文DTO
     */
    public void validateAndEnrichCheckout(TradeDTO tradeDTO, AppointmentCheckoutDTO checkout) {
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
        if (!appointmentSlotStockSupport.isSlotStartAfterNow(stock.getSlotDate(), stock.getSlotStartTime())) {
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
        AppointmentGoodsExt goodsExt = goodsExtMapper.selectOne(new LambdaQueryWrapper<AppointmentGoodsExt>()
                .eq(AppointmentGoodsExt::getGoodsId, line.getGoodsSku().getGoodsId())
                .eq(AppointmentGoodsExt::getDeleteFlag, false));
        if (!appointmentSlotStockSupport.isBookableSlotDate(goodsExt, stock.getSlotDate())) {
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

    /**
     * 判断店铺是否启用了上门服务区域电子围栏
     *
     * @param storeId 店铺ID
     * @return 是否开启
     */
    public boolean isServiceAreaEnabled(String storeId) {
        return serviceAreaPolygons(storeId) != null;
    }

    /**
     * 校验预约服务地址坐标是否落在服务区域多边形内
     *
     * @param storeId  店铺ID
     * @param checkout 结算参数
     */
    public void assertInsideServiceArea(String storeId, AppointmentCheckoutDTO checkout) {
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

    /**
     * 解析店铺上门服务区域多边形配置
     *
     * @param storeId 店铺ID
     * @return 多边形顶点列表
     */
    public List<List<double[]>> serviceAreaPolygons(String storeId) {
        if (CharSequenceUtil.isBlank(storeId)) {
            return null;
        }
        AppointmentStoreConfig config = appointmentConfigSupport.loadStoreConfig(storeId, AppointmentServiceModeEnum.ON_SITE.name());
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

    /**
     * 从 TradeDTO 或本地线程上下文中获取预约结算上下文
     *
     * @param tradeDTO 交易DTO
     * @return 结算上下文
     */
    public AppointmentCheckoutDTO resolveCheckout(TradeDTO tradeDTO) {
        if (tradeDTO == null) {
            return null;
        }
        if (tradeDTO.getAppointmentCheckout() != null) {
            return tradeDTO.getAppointmentCheckout();
        }
        return AppointmentCheckoutContextHolder.get(tradeDTO.getMemberId());
    }

    /**
     * 构建预约表单答案JSON
     *
     * @param checkout 结算上下文
     * @param goodsExt 预约商品扩展
     * @param quantity 购买数量
     * @return 标准化答案JSON
     */
    public String buildFormAnswerJson(AppointmentCheckoutDTO checkout, AppointmentGoodsExt goodsExt, int quantity) {
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

    /**
     * 构建退订规则快照JSON
     *
     * @param goodsExt 预约商品扩展
     * @return 快照JSON
     */
    public String buildCancelPolicySnapshot(AppointmentGoodsExt goodsExt) {
        if (goodsExt == null) {
            return null;
        }
        JSONObject json = new JSONObject();
        json.set("cancelMode", goodsExt.getCancelMode());
        json.set("cancelHours", goodsExt.getCancelHours());
        return json.toString();
    }

    /**
     * 从主订单或已落库扩展中恢复预约结算上下文
     *
     * @param order 主订单
     * @return 结算上下文
     */
    public AppointmentCheckoutDTO resolveCheckoutFromOrder(Order order) {
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

    /**
     * 持久化订单预约扩展记录
     *
     * @param orderSn      订单编号
     * @param checkoutHint 预约上下文线索（可为空）
     */
    @Transactional(rollbackFor = Exception.class)
    public void persistOrderExt(String orderSn, AppointmentCheckoutDTO checkoutHint) {
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
        AppointmentGoodsExt goodsExt = goodsExtMapper.selectOne(new LambdaQueryWrapper<AppointmentGoodsExt>()
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

    /**
     * 支付成功后的核心履约流程（锁订单、扣减时段库存、生成工单与核销码、推进订单状态）
     *
     * @param orderSn 订单编号
     */
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
            persistOrderExt(orderSn, null);
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
            appointmentVerificationSupport.transitionOrderToTake(orderSn);
            return;
        }
        int quantity = orderExt.getQuantity() == null ? 1 : orderExt.getQuantity();
        if (!appointmentSlotStockSupport.deductSlotStock(orderExt.getSlotStockId(), quantity)) {
            failFulfillment(orderSn, orderExt.getSlotStockId(), quantity);
            throw new ServiceException(ResultCode.APPOINTMENT_SLOT_INSUFFICIENT);
        }
        try {
            createWorkOrderAndCodes(order, orderExt, quantity);
            appointmentVerificationSupport.transitionOrderToTake(orderSn);
            appointmentAfterSaleSupport.notifyBookingSuccess(orderSn);
        } catch (Exception e) {
            log.error("预约履约失败，orderSn={}", orderSn, e);
            appointmentSlotStockSupport.restoreSlotStock(orderExt.getSlotStockId(), quantity);
            failFulfillment(orderSn, orderExt.getSlotStockId(), quantity);
            throw e;
        }
    }

    /**
     * 生成工单与核销码
     *
     * @param order    主订单
     * @param orderExt 预约扩展实体
     * @param quantity 预约数量
     */
    public void createWorkOrderAndCodes(Order order, AppointmentOrderExt orderExt, int quantity) {
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
        workOrder.setAppointmentTime(appointmentSlotStockSupport.buildAppointmentDateTime(orderExt));
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
            code.setCode(appointmentVerificationSupport.generateUniqueCode(order.getStoreId(), usedCodes));
            code.setDeleteFlag(false);
            verificationCodeMapper.insert(code);
        }
    }

    /**
     * 履约失败兜底取消
     *
     * @param orderSn     订单编号
     * @param slotStockId 时段库存ID
     * @param quantity    数量
     */
    public void failFulfillment(String orderSn, String slotStockId, int quantity) {
        if (CharSequenceUtil.isNotBlank(slotStockId) && quantity > 0) {
            appointmentSlotStockSupport.restoreSlotStock(slotStockId, quantity);
        }
        orderService.systemCancel(orderSn, FULFILL_FAIL_CANCEL_REASON, true);
    }
}
