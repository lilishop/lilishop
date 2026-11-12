package cn.lili.modules.appointment.support;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.appointment.entity.dos.AppointmentGoodsExt;
import cn.lili.modules.appointment.entity.dos.AppointmentOrderExt;
import cn.lili.modules.appointment.entity.dos.AppointmentSlotStock;
import cn.lili.modules.appointment.entity.dos.AppointmentSlotTemplate;
import cn.lili.modules.appointment.entity.dos.AppointmentWorkOrder;
import cn.lili.modules.appointment.entity.vo.AppointmentCalendarDaySummaryVO;
import cn.lili.modules.appointment.entity.vo.AppointmentCalendarEntryVO;
import cn.lili.modules.appointment.entity.vo.AppointmentSlotStockVO;
import cn.lili.modules.appointment.mapper.AppointmentGoodsExtMapper;
import cn.lili.modules.appointment.mapper.AppointmentOrderExtMapper;
import cn.lili.modules.appointment.mapper.AppointmentSlotStockMapper;
import cn.lili.modules.appointment.mapper.AppointmentSlotTemplateMapper;
import cn.lili.modules.appointment.mapper.AppointmentWorkOrderMapper;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.mapper.OrderMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 预约时段模板、库存与日历看板业务支持组件
 * <p>
 * 负责可用时段实时计算、时段库存按需自动初始化、时段扣减与释放，以及月度/按日看板装配。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Component
public class AppointmentSlotStockSupport {

    @Autowired
    private AppointmentSlotTemplateMapper slotTemplateMapper;

    @Autowired
    private AppointmentSlotStockMapper slotStockMapper;

    @Autowired
    private AppointmentOrderExtMapper orderExtMapper;

    @Autowired
    private AppointmentWorkOrderMapper workOrderMapper;

    @Autowired
    private AppointmentGoodsExtMapper goodsExtMapper;

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    private OrderMapper orderMapper;

    @Autowired
    private AppointmentStaffSupport appointmentStaffSupport;

    /**
     * 查询指定 SKU 在某日期的可用预约时段列表
     *
     * @param skuId    商品SKU ID
     * @param slotDate 预约日期
     * @return 可用时段VO列表
     */
    public List<AppointmentSlotStockVO> listAvailableSlots(String skuId, Date slotDate) {
        GoodsSku sku = goodsSkuService.getById(skuId);
        if (sku == null || !GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(sku.getGoodsType())) {
            throw new ServiceException(ResultCode.APPOINTMENT_SKU_NOT_FOUND);
        }
        AppointmentGoodsExt ext = goodsExtMapper.selectOne(new LambdaQueryWrapper<AppointmentGoodsExt>()
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

    /**
     * 扣减指定时段库存（乐观锁/行级排他锁）
     *
     * @param slotStockId 时段库存ID
     * @param quantity    扣减数量
     * @return 是否扣减成功
     */
    public boolean deductSlotStock(String slotStockId, int quantity) {
        if (CharSequenceUtil.isBlank(slotStockId) || quantity <= 0) {
            return false;
        }
        slotStockMapper.selectForUpdateById(slotStockId);
        return slotStockMapper.deductAvailableQty(slotStockId, quantity) > 0;
    }

    /**
     * 返还/回滚指定时段库存
     *
     * @param slotStockId 时段库存ID
     * @param quantity    回滚数量
     */
    public void restoreSlotStock(String slotStockId, int quantity) {
        if (CharSequenceUtil.isBlank(slotStockId) || quantity <= 0) {
            return;
        }
        slotStockMapper.restoreAvailableQty(slotStockId, quantity);
    }

    /**
     * 预约看板月度预约人数按日汇总
     *
     * @param storeId 店铺ID
     * @param month   月份（yyyy-MM）
     * @return 每日汇总数据
     */
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

    /**
     * 预约看板某日预约条目明细
     *
     * @param storeId 店铺ID
     * @param date    日期
     * @param status  筛选状态
     * @return 日程条目列表
     */
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
        Map<String, String> staffNameMap = appointmentStaffSupport.loadStaffNameMap(workOrderMap.values());

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

    /**
     * 确保指定日期的时段库存已从模板实例化
     *
     * @param sku      商品SKU
     * @param ext      预约扩展设置
     * @param slotDate 预约日期
     */
    public void ensureSlotStockForDate(GoodsSku sku, AppointmentGoodsExt ext, Date slotDate) {
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

    /**
     * 判断日期是否在商品允许预约的周期范围内
     *
     * @param ext      预约设置
     * @param slotDate 待校验日期
     * @return 是否可约
     */
    public boolean isBookableSlotDate(AppointmentGoodsExt ext, Date slotDate) {
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

    /**
     * 解析自定义星期集合
     *
     * @param raw 逗号分隔的星期字符串（1~7）
     * @return 星期集合
     */
    public Set<Integer> parseCustomWeekdays(String raw) {
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

    /**
     * Date 转换为 LocalDate
     *
     * @param date 日期
     * @return LocalDate
     */
    public LocalDate toLocalDate(Date date) {
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        return LocalDate.of(
                calendar.get(Calendar.YEAR),
                calendar.get(Calendar.MONTH) + 1,
                calendar.get(Calendar.DAY_OF_MONTH));
    }

    /**
     * 判断时段开始时间是否在当前时间之后
     *
     * @param slotDate      预约日期
     * @param slotStartTime 时段开始时间（HH:mm）
     * @return 是否在将来
     */
    public boolean isSlotStartAfterNow(Date slotDate, String slotStartTime) {
        if (slotDate == null || CharSequenceUtil.isBlank(slotStartTime)) {
            return false;
        }
        Date start = DateUtil.parse(DateUtil.formatDate(slotDate) + " " + slotStartTime + ":00");
        return start != null && start.after(new Date());
    }

    /**
     * 构建预约开始的完整时间戳
     *
     * @param orderExt 预约扩展实体
     * @return 完整Date
     */
    public Date buildAppointmentDateTime(AppointmentOrderExt orderExt) {
        String datePart = DateUtil.formatDate(orderExt.getSlotDate());
        return DateUtil.parse(datePart + " " + orderExt.getSlotStartTime() + ":00");
    }

    /**
     * 将主订单状态映射为日历看板展示状态
     *
     * @param order 主订单
     * @return 看板状态字串
     */
    public String resolveCalendarStatus(Order order) {
        if (order == null || order.getOrderStatus() == null) {
            return "UNKNOWN";
        }
        if (OrderStatusEnum.PAID.name().equals(order.getOrderStatus())) {
            return "WAIT_VERIFY";
        }
        if (OrderStatusEnum.TAKE.name().equals(order.getOrderStatus())) {
            return "IN_SERVICE";
        }
        if (OrderStatusEnum.COMPLETED.name().equals(order.getOrderStatus())) {
            return "SERVICE_ENDED";
        }
        return order.getOrderStatus();
    }
}
