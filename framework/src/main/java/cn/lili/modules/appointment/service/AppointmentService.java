package cn.lili.modules.appointment.service;

import cn.lili.modules.appointment.entity.dos.AppointmentVerificationCode;
import cn.lili.modules.appointment.entity.dos.AppointmentServiceStaff;
import cn.lili.modules.appointment.entity.dos.AppointmentStoreConfig;
import cn.lili.modules.appointment.entity.dos.AppointmentSystemForm;
import cn.lili.modules.appointment.entity.dto.AppointmentCheckoutDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentGoodsExtSaveDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentNoShowRefundDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentPlatformRefundDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentReassignDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentServiceStaffDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentStoreConfigDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentSystemFormDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentVerifyCodeDTO;
import cn.lili.modules.appointment.entity.dto.search.AppointmentWorkOrderSearchParams;
import cn.lili.modules.appointment.entity.vo.AppointmentBuyerOrderDetailVO;
import cn.lili.modules.appointment.entity.vo.AppointmentCalendarDaySummaryVO;
import cn.lili.modules.appointment.entity.vo.AppointmentCalendarEntryVO;
import cn.lili.modules.appointment.entity.vo.AppointmentGoodsExtVO;
import cn.lili.modules.appointment.entity.vo.AppointmentSlotStockVO;
import cn.lili.modules.appointment.entity.vo.AppointmentVerificationOrderVO;
import cn.lili.modules.appointment.entity.vo.AppointmentVerificationResultVO;
import cn.lili.modules.appointment.entity.vo.AppointmentWorkOrderVO;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.system.entity.dto.AppointmentOrderSetting;
import com.baomidou.mybatisplus.core.metadata.IPage;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 预约服务商品核心业务层
 *
 * @author Bulbasaur
 * @since 2026-09-18
 */
public interface AppointmentService {

    /**
     * 校验加购/立即购买：预约商品仅允许立即购买
     *
     * @param skuId    SKU ID
     * @param num      购买数量
     * @param cartType 购物车类型
     */
    void validateAddToCart(String skuId, Integer num, String cartType);

    /**
     * 创单前校验：禁混单、校验时段与提前预约
     *
     * @param tradeDTO 交易信息
     */
    void validateTrade(TradeDTO tradeDTO);

    /**
     * 支付前校验预约单
     *
     * @param order 订单
     */
    void validatePayOrder(Order order);

    /**
     * 当前交易是否全部为预约商品
     *
     * @param tradeDTO 交易信息
     * @return 是否预约交易
     */
    boolean isAppointmentOnlyTrade(TradeDTO tradeDTO);

    /**
     * 归一化预约交易（免物流等）
     *
     * @param tradeDTO 交易信息
     */
    void normalizeAppointmentTrade(TradeDTO tradeDTO);

    /**
     * 查询预约商品扩展设置
     *
     * @param goodsId 商品ID
     * @return 预约扩展
     */
    AppointmentGoodsExtVO getGoodsExt(String goodsId);

    /**
     * 保存预约商品扩展设置
     *
     * @param dto 保存参数
     */
    void saveGoodsExt(AppointmentGoodsExtSaveDTO dto);

    /**
     * 发品/编辑商品时同步预约设置与时段模板
     *
     * @param goodsId            商品ID
     * @param goodsOperationDTO 商品操作参数
     */
    void syncGoodsExtFromPublish(String goodsId, GoodsOperationDTO goodsOperationDTO);

    /**
     * 商家编辑回显：填充预约设置、时段模板、SKU 可约数量
     *
     * @param goodsVO 商品 VO
     */
    void enrichGoodsVoForSeller(GoodsVO goodsVO);

    /**
     * 查询指定日期可预约时段
     *
     * @param skuId    SKU ID
     * @param slotDate 预约日期
     * @return 时段库存列表
     */
    List<AppointmentSlotStockVO> listAvailableSlots(String skuId, Date slotDate);

    /**
     * 绑定预约结算上下文：校验时段与补充信息，写入交易缓存（与发票 receiptVO 同路径）
     *
     * @param tradeDTO 交易信息
     * @param checkout 预约结算参数
     */
    void bindCheckoutContext(TradeDTO tradeDTO, AppointmentCheckoutDTO checkout);

    /**
     * 创单时写入订单扩展（时段快照 + 补充信息）；不扣时段，也不写入缓存
     *
     * @param tradeDTO 已写入主单的交易
     */
    void stashCheckoutForPayment(TradeDTO tradeDTO);

    /**
     * 订单创建消息：按发票同样时机，把时段与补充信息写入订单扩展
     *
     * @param tradeDTO 交易（含 appointmentCheckout）
     */
    void persistOrderExtFromTrade(TradeDTO tradeDTO);

    /**
     * 落库预约订单扩展（不扣时段）：记录所选时段与补充信息答案
     *
     * @param orderSn 订单编号
     */
    void persistOrderExt(String orderSn);

    /**
     * 支付成功履约：读取创单时写入的扩展，扣时段、发码、生成工单
     *
     * @param orderSn 订单编号
     */
    void fulfillAfterPayment(String orderSn);

    /**
     * 扣减时段可约数量
     *
     * @param slotStockId 时段库存ID
     * @param quantity    扣减数量
     * @return 是否扣减成功
     */
    boolean deductSlotStock(String slotStockId, int quantity);

    /**
     * 回补时段可约数量
     *
     * @param slotStockId 时段库存ID
     * @param quantity    回补数量
     */
    void restoreSlotStock(String slotStockId, int quantity);

    /**
     * 服务人员分页
     *
     * @param storeId 店铺ID
     * @param params  查询参数
     * @return 分页结果
     */
    IPage<AppointmentServiceStaff> pageStaff(String storeId, AppointmentWorkOrderSearchParams params);

    /**
     * 启用中的服务人员列表
     *
     * @param storeId 店铺ID
     * @return 服务人员
     */
    List<AppointmentServiceStaff> listEnabledStaff(String storeId);

    /**
     * 保存服务人员
     *
     * @param storeId 店铺ID
     * @param dto     保存参数
     * @return 服务人员
     */
    AppointmentServiceStaff saveStaff(String storeId, AppointmentServiceStaffDTO dto);

    /**
     * 删除服务人员
     *
     * @param storeId 店铺ID
     * @param staffId 服务人员ID
     */
    void deleteStaff(String storeId, String staffId);

    /**
     * 店铺履约配置列表
     *
     * @param storeId 店铺ID
     * @return 配置列表
     */
    List<AppointmentStoreConfig> listStoreConfig(String storeId);

    /**
     * 保存店铺履约配置
     *
     * @param storeId 店铺ID
     * @param dto     配置参数
     */
    void saveStoreConfig(String storeId, AppointmentStoreConfigDTO dto);

    /**
     * 工单分页
     *
     * @param params        查询参数
     * @param managerScope  是否运营端全平台
     * @return 分页结果
     */
    IPage<AppointmentWorkOrderVO> pageWorkOrders(AppointmentWorkOrderSearchParams params, boolean managerScope);

    /**
     * 工单状态数量统计
     *
     * @param storeId 店铺ID
     * @return 状态-数量
     */
    Map<String, Long> countWorkOrderStatus(String storeId);

    /**
     * 按核销码查询预约订单
     *
     * @param storeId 店铺ID
     * @param code    核销码
     * @return 核销订单信息
     */
    AppointmentVerificationOrderVO getOrderByVerificationCode(String storeId, String code);

    /**
     * 按订单号查询预约核销信息
     *
     * @param storeId 店铺ID
     * @param orderSn 订单编号
     * @return 核销订单信息
     */
    AppointmentVerificationOrderVO getVerificationByOrderSn(String storeId, String orderSn);

    /**
     * 核销预约码（一次须核完全部 N 码）
     *
     * @param storeId 店铺ID
     * @param userId  操作人
     * @param dto     核销参数
     * @return 核销结果
     */
    AppointmentVerificationResultVO verifyCode(String storeId, String userId, AppointmentVerifyCodeDTO dto);

    /**
     * 本店是否存在该核销码（含已使用）
     *
     * @param storeId 店铺ID
     * @param code    核销码
     * @return 是否存在
     */
    boolean hasVerificationCode(String storeId, String code);

    /**
     * 查询本店预约核销码，不存在时返回 null。
     *
     * @param storeId 店铺ID
     * @param code    核销码
     * @return 核销码
     */
    AppointmentVerificationCode findVerificationCode(String storeId, String code);

    /**
     * 按核销码核销，不指定服务人员。核销员记为当前商家账号。
     *
     * @param storeId 店铺ID
     * @param userId  操作人
     * @param code    核销码
     * @return 核销结果
     */
    AppointmentVerificationResultVO verifyCodeByOperator(String storeId, String userId, String code);

    /**
     * 结束服务
     *
     * @param storeId     店铺ID
     * @param workOrderId 工单ID
     */
    void endService(String storeId, String workOrderId);

    /**
     * 领取上门工单
     *
     * @param storeId     店铺ID
     * @param userId      操作人
     * @param workOrderId 工单ID
     */
    void claimWorkOrder(String storeId, String userId, String workOrderId);

    /**
     * 转派工单
     *
     * @param storeId     店铺ID
     * @param dto         转派参数
     * @param workOrderId 工单ID
     */
    void reassignWorkOrder(String storeId, AppointmentReassignDTO dto, String workOrderId);

    /**
     * 服务月历按日汇总
     *
     * @param storeId 店铺ID
     * @param month   月份 yyyy-MM
     * @return 按日汇总
     */
    List<AppointmentCalendarDaySummaryVO> calendarMonthSummary(String storeId, String month);

    /**
     * 服务月历当日明细
     *
     * @param storeId 店铺ID
     * @param date    日期
     * @param status  工单状态，ALL 表示全部
     * @return 当日工单
     */
    List<AppointmentCalendarEntryVO> calendarDayEntries(String storeId, Date date, String status);

    /**
     * 到店服务设置
     *
     * @param storeId 店铺ID
     * @return 设置表单
     */
    Map<String, Object> getInStoreSettings(String storeId);

    /**
     * 保存到店服务设置
     *
     * @param storeId 店铺ID
     * @param form    设置表单
     */
    void saveInStoreSettings(String storeId, Map<String, Object> form);

    /**
     * 上门服务设置
     *
     * @param storeId 店铺ID
     * @return 设置表单
     */
    Map<String, Object> getOnSiteSettings(String storeId);

    /**
     * 保存上门服务设置
     *
     * @param storeId 店铺ID
     * @param form    设置表单
     */
    void saveOnSiteSettings(String storeId, Map<String, Object> form);

    /**
     * 买家预约订单详情
     *
     * @param memberId 会员ID
     * @param orderSn  订单编号
     * @return 预约详情
     */
    AppointmentBuyerOrderDetailVO getBuyerOrderDetail(String memberId, String orderSn);

    /**
     * 预约订单扩展详情（商家/运营查看时段与补充信息）
     *
     * @param orderSn 订单编号
     * @return 预约详情
     */
    AppointmentBuyerOrderDetailVO getOrderExtDetail(String orderSn);

    /**
     * 是否预约订单
     *
     * @param order 订单
     * @return 是否预约单
     */
    boolean isAppointmentOrder(Order order);

    /**
     * 服务是否已完结
     *
     * @param orderSn 订单编号
     * @return 是否完结
     */
    boolean isServiceComplete(String orderSn);

    /**
     * 买家是否可申请售后
     *
     * @param orderSn 订单编号
     * @return 是否可售后
     */
    boolean canApplyBuyerAfterSale(String orderSn);

    /**
     * 剩余可退金额
     *
     * @param orderSn 订单编号
     * @return 可退金额
     */
    double getRemainingRefundableAmount(String orderSn);

    /**
     * 校验买家售后申请金额
     *
     * @param orderSn     订单编号
     * @param applyAmount 申请金额
     */
    void validateBuyerAfterSaleAmount(String orderSn, double applyAmount);

    /**
     * 退款成功回调：部分退不关单，退尽关单并回补时段
     *
     * @param orderSn      订单编号
     * @param refundAmount 本次退款金额
     */
    void onRefundSuccess(String orderSn, double refundAmount);

    /**
     * 平台预约退款
     *
     * @param orderSn 订单编号
     * @param dto     退款参数
     */
    void platformRefund(String orderSn, AppointmentPlatformRefundDTO dto);

    /**
     * 上门爽约退款
     *
     * @param orderSn 订单编号
     * @param dto     退款参数
     */
    void noShowRefund(String orderSn, AppointmentNoShowRefundDTO dto);

    /**
     * 系统表单分页
     *
     * @param storeId    店铺ID
     * @param pageNumber 页码
     * @param pageSize   页大小
     * @return 分页结果
     */
    IPage<AppointmentSystemForm> pageSystemForms(String storeId, int pageNumber, int pageSize,
                                                 String formName, String status);

    /**
     * 读取本店系统表单详情
     *
     * @param storeId 店铺ID
     * @param formId  表单ID
     * @return 表单
     */
    AppointmentSystemForm getSystemForm(String storeId, String formId);

    /**
     * 保存系统表单
     *
     * @param storeId 店铺ID
     * @param dto     表单参数
     * @return 表单
     */
    AppointmentSystemForm saveSystemForm(String storeId, AppointmentSystemFormDTO dto);

    /**
     * 删除系统表单
     *
     * @param storeId 店铺ID
     * @param formId  表单ID
     */
    void deleteSystemForm(String storeId, String formId);

    /**
     * 启用中的系统表单
     *
     * @param storeId 店铺ID
     * @return 表单列表
     */
    List<AppointmentSystemForm> listEnabledSystemForms(String storeId);

    /**
     * 读取平台预约订单设置
     *
     * @return 预约订单设置
     */
    AppointmentOrderSetting getAppointmentOrderSetting();

    /**
     * 保存平台预约订单设置
     *
     * @param setting 预约订单设置
     */
    void saveAppointmentOrderSetting(AppointmentOrderSetting setting);

    /**
     * 逾期未核销自动完结
     */
    void processNoShowAutoComplete();

    /**
     * 服务开始前提醒买家
     */
    void processServiceStartRemind();

    /**
     * 预约成功站内信
     *
     * @param orderSn 订单编号
     */
    void notifyBookingSuccess(String orderSn);
}
