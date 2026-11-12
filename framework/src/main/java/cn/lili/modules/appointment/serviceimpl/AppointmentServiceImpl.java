package cn.lili.modules.appointment.serviceimpl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.appointment.entity.dos.*;
import cn.lili.modules.appointment.entity.dto.*;
import cn.lili.modules.appointment.entity.dto.search.AppointmentWorkOrderSearchParams;
import cn.lili.modules.appointment.entity.vo.*;
import cn.lili.modules.appointment.mapper.*;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.appointment.support.*;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.vo.OrderVO;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.StoreService;
import cn.lili.modules.system.entity.dto.AppointmentOrderSetting;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * 预约服务商品核心服务实现
 * <p>
 * 采用门面模式（Facade）对外提供统一的预约服务能力，各细分子域分别委托给对应的 Support 业务组件处理。
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Slf4j
@Service
public class AppointmentServiceImpl extends ServiceImpl<AppointmentGoodsExtMapper, AppointmentGoodsExt>
        implements AppointmentService {

    @Autowired
    private AppointmentSlotTemplateMapper slotTemplateMapper;

    @Autowired
    private AppointmentSystemFormMapper systemFormMapper;

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    private GoodsService goodsService;

    @Autowired
    private StoreService storeService;

    @Autowired
    private AppointmentStaffSupport appointmentStaffSupport;

    @Autowired
    private AppointmentConfigSupport appointmentConfigSupport;

    @Autowired
    private AppointmentWorkOrderSupport appointmentWorkOrderSupport;

    @Autowired
    private AppointmentSlotStockSupport appointmentSlotStockSupport;

    @Autowired
    private AppointmentVerificationSupport appointmentVerificationSupport;

    @Autowired
    private AppointmentAfterSaleSupport appointmentAfterSaleSupport;

    @Autowired
    private AppointmentCheckoutSupport appointmentCheckoutSupport;

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
        appointmentCheckoutSupport.validateAndEnrichCheckout(tradeDTO, checkout);
        tradeDTO.setAppointmentCheckout(checkout);
        AppointmentCheckoutContextHolder.bind(tradeDTO.getMemberId(), checkout);
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
        vo.setServiceAreaRequired(appointmentCheckoutSupport.isServiceAreaEnabled(ext.getStoreId()));
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

    @Override
    public List<AppointmentSlotStockVO> listAvailableSlots(String skuId, Date slotDate) {
        return appointmentSlotStockSupport.listAvailableSlots(skuId, slotDate);
    }

    @Override
    public void bindCheckoutContext(TradeDTO tradeDTO, AppointmentCheckoutDTO checkout) {
        if (tradeDTO == null || checkout == null) {
            throw new ServiceException(ResultCode.APPOINTMENT_CHECKOUT_CONTEXT_MISSING);
        }
        appointmentCheckoutSupport.validateAndEnrichCheckout(tradeDTO, checkout);
        tradeDTO.setAppointmentCheckout(checkout);
        AppointmentCheckoutContextHolder.bind(tradeDTO.getMemberId(), checkout);
    }

    @Override
    public void stashCheckoutForPayment(TradeDTO tradeDTO) {
        if (!isAppointmentOnlyTrade(tradeDTO) || CollUtil.isEmpty(tradeDTO.getOrderVO())) {
            return;
        }
        AppointmentCheckoutDTO checkout = appointmentCheckoutSupport.resolveCheckout(tradeDTO);
        if (checkout == null) {
            throw new ServiceException(ResultCode.APPOINTMENT_CHECKOUT_CONTEXT_MISSING);
        }
        tradeDTO.setAppointmentCheckout(checkout);
        for (OrderVO orderVO : tradeDTO.getOrderVO()) {
            if (orderVO == null || CharSequenceUtil.isBlank(orderVO.getSn())) {
                continue;
            }
            appointmentCheckoutSupport.persistOrderExt(orderVO.getSn(), checkout);
        }
        AppointmentCheckoutContextHolder.remove(tradeDTO.getMemberId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void persistOrderExtFromTrade(TradeDTO tradeDTO) {
        if (tradeDTO == null || !isAppointmentOnlyTrade(tradeDTO) || CollUtil.isEmpty(tradeDTO.getOrderVO())) {
            return;
        }
        AppointmentCheckoutDTO checkout = appointmentCheckoutSupport.resolveCheckout(tradeDTO);
        for (OrderVO orderVO : tradeDTO.getOrderVO()) {
            if (orderVO == null || CharSequenceUtil.isBlank(orderVO.getSn())) {
                continue;
            }
            appointmentCheckoutSupport.persistOrderExt(orderVO.getSn(), checkout);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void persistOrderExt(String orderSn) {
        appointmentCheckoutSupport.persistOrderExt(orderSn, null);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void fulfillAfterPayment(String orderSn) {
        appointmentCheckoutSupport.fulfillAfterPayment(orderSn);
    }

    @Override
    public boolean deductSlotStock(String slotStockId, int quantity) {
        return appointmentSlotStockSupport.deductSlotStock(slotStockId, quantity);
    }

    @Override
    public void restoreSlotStock(String slotStockId, int quantity) {
        appointmentSlotStockSupport.restoreSlotStock(slotStockId, quantity);
    }

    @Override
    public IPage<AppointmentServiceStaff> pageStaff(String storeId, AppointmentWorkOrderSearchParams params) {
        return appointmentStaffSupport.pageStaff(storeId, params);
    }

    @Override
    public List<AppointmentServiceStaff> listEnabledStaff(String storeId) {
        return appointmentStaffSupport.listEnabledStaff(storeId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public AppointmentServiceStaff saveStaff(String storeId, AppointmentServiceStaffDTO dto) {
        return appointmentStaffSupport.saveStaff(storeId, dto);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteStaff(String storeId, String staffId) {
        appointmentStaffSupport.deleteStaff(storeId, staffId);
    }

    @Override
    public List<AppointmentStoreConfig> listStoreConfig(String storeId) {
        return appointmentConfigSupport.listStoreConfig(storeId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveStoreConfig(String storeId, AppointmentStoreConfigDTO dto) {
        appointmentConfigSupport.saveStoreConfig(storeId, dto);
    }

    @Override
    public IPage<AppointmentWorkOrderVO> pageWorkOrders(AppointmentWorkOrderSearchParams params, boolean managerScope) {
        return appointmentWorkOrderSupport.pageWorkOrders(params, managerScope);
    }

    @Override
    public AppointmentVerificationOrderVO getOrderByVerificationCode(String storeId, String code) {
        return appointmentVerificationSupport.getOrderByVerificationCode(storeId, code);
    }

    @Override
    public AppointmentVerificationOrderVO getVerificationByOrderSn(String storeId, String orderSn) {
        return appointmentVerificationSupport.getVerificationByOrderSn(storeId, orderSn);
    }

    @Override
    public AppointmentBuyerOrderDetailVO getBuyerOrderDetail(String memberId, String orderSn) {
        return appointmentVerificationSupport.getBuyerOrderDetail(memberId, orderSn);
    }

    @Override
    public AppointmentBuyerOrderDetailVO getOrderExtDetail(String orderSn) {
        return appointmentVerificationSupport.getOrderExtDetail(orderSn);
    }

    @Override
    public boolean hasVerificationCode(String storeId, String code) {
        return appointmentVerificationSupport.hasVerificationCode(storeId, code);
    }

    @Override
    public AppointmentVerificationCode findVerificationCode(String storeId, String code) {
        return appointmentVerificationSupport.findVerificationCode(storeId, code);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public AppointmentVerificationResultVO verifyCodeByOperator(String storeId, String userId, String code) {
        return appointmentVerificationSupport.verifyCodeByOperator(storeId, userId, code);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public AppointmentVerificationResultVO verifyCode(String storeId, String userId, AppointmentVerifyCodeDTO dto) {
        return appointmentVerificationSupport.verifyCode(storeId, userId, dto);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void endService(String storeId, String workOrderId) {
        appointmentWorkOrderSupport.endService(storeId, workOrderId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void claimWorkOrder(String storeId, String userId, String workOrderId) {
        appointmentWorkOrderSupport.claimWorkOrder(storeId, userId, workOrderId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void reassignWorkOrder(String storeId, AppointmentReassignDTO dto, String workOrderId) {
        appointmentWorkOrderSupport.reassignWorkOrder(storeId, dto, workOrderId);
    }

    @Override
    public Map<String, Long> countWorkOrderStatus(String storeId) {
        return appointmentWorkOrderSupport.countWorkOrderStatus(storeId);
    }

    @Override
    public List<AppointmentCalendarDaySummaryVO> calendarMonthSummary(String storeId, String month) {
        return appointmentSlotStockSupport.calendarMonthSummary(storeId, month);
    }

    @Override
    public List<AppointmentCalendarEntryVO> calendarDayEntries(String storeId, Date date, String status) {
        return appointmentSlotStockSupport.calendarDayEntries(storeId, date, status);
    }

    @Override
    public Map<String, Object> getInStoreSettings(String storeId) {
        return appointmentConfigSupport.getInStoreSettings(storeId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveInStoreSettings(String storeId, Map<String, Object> form) {
        appointmentConfigSupport.saveInStoreSettings(storeId, form);
    }

    @Override
    public Map<String, Object> getOnSiteSettings(String storeId) {
        return appointmentConfigSupport.getOnSiteSettings(storeId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveOnSiteSettings(String storeId, Map<String, Object> form) {
        appointmentConfigSupport.saveOnSiteSettings(storeId, form);
    }

    @Override
    public boolean isAppointmentOrder(Order order) {
        return appointmentAfterSaleSupport.isAppointmentOrder(order);
    }

    @Override
    public boolean isServiceComplete(String orderSn) {
        return appointmentAfterSaleSupport.isServiceComplete(orderSn);
    }

    @Override
    public boolean canApplyBuyerAfterSale(String orderSn) {
        return appointmentAfterSaleSupport.canApplyBuyerAfterSale(orderSn);
    }

    @Override
    public double getRemainingRefundableAmount(String orderSn) {
        return appointmentAfterSaleSupport.getRemainingRefundableAmount(orderSn);
    }

    @Override
    public void validateBuyerAfterSaleAmount(String orderSn, double applyAmount) {
        appointmentAfterSaleSupport.validateBuyerAfterSaleAmount(orderSn, applyAmount);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void onRefundSuccess(String orderSn, double refundAmount) {
        appointmentAfterSaleSupport.onRefundSuccess(orderSn, refundAmount);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void platformRefund(String orderSn, AppointmentPlatformRefundDTO dto) {
        appointmentAfterSaleSupport.platformRefund(orderSn, dto);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void noShowRefund(String orderSn, AppointmentNoShowRefundDTO dto) {
        appointmentAfterSaleSupport.noShowRefund(orderSn, dto);
    }

    @Override
    public IPage<AppointmentSystemForm> pageSystemForms(String storeId, int pageNumber, int pageSize,
                                                        String formName, String status) {
        return appointmentConfigSupport.pageSystemForms(storeId, pageNumber, pageSize, formName, status);
    }

    @Override
    public AppointmentSystemForm getSystemForm(String storeId, String formId) {
        return appointmentConfigSupport.getSystemForm(storeId, formId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public AppointmentSystemForm saveSystemForm(String storeId, AppointmentSystemFormDTO dto) {
        return appointmentConfigSupport.saveSystemForm(storeId, dto);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteSystemForm(String storeId, String formId) {
        appointmentConfigSupport.deleteSystemForm(storeId, formId);
    }

    @Override
    public List<AppointmentSystemForm> listEnabledSystemForms(String storeId) {
        return appointmentConfigSupport.listEnabledSystemForms(storeId);
    }

    @Override
    public AppointmentOrderSetting getAppointmentOrderSetting() {
        return appointmentConfigSupport.getAppointmentOrderSetting();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveAppointmentOrderSetting(AppointmentOrderSetting setting) {
        appointmentConfigSupport.saveAppointmentOrderSetting(setting);
    }

    @Override
    public void processNoShowAutoComplete() {
        appointmentAfterSaleSupport.processNoShowAutoComplete();
    }

    @Override
    public void processServiceStartRemind() {
        appointmentAfterSaleSupport.processServiceStartRemind();
    }

    @Override
    public void notifyBookingSuccess(String orderSn) {
        appointmentAfterSaleSupport.notifyBookingSuccess(orderSn);
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
}
