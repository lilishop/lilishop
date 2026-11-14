package cn.lili.modules.appointment.support;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.appointment.entity.dos.AppointmentWorkOrder;
import cn.lili.modules.appointment.entity.dto.AppointmentReassignDTO;
import cn.lili.modules.appointment.entity.dto.search.AppointmentWorkOrderSearchParams;
import cn.lili.modules.appointment.entity.enums.AppointmentWorkOrderStatusEnum;
import cn.lili.modules.appointment.entity.vo.AppointmentWorkOrderVO;
import cn.lili.modules.appointment.mapper.AppointmentWorkOrderMapper;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.support.OrderLifecycleSupport;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 预约工单流转与派单业务支持组件
 * <p>
 * 负责预约工单的分页查询、认领、改派、状态完结与按状态统计。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Component
public class AppointmentWorkOrderSupport {

    @Autowired
    private AppointmentWorkOrderMapper workOrderMapper;

    @Autowired
    private OrderMapper orderMapper;

    @Autowired
    private OrderLifecycleSupport orderLifecycleSupport;

    @Autowired
    private StoreService storeService;

    @Autowired
    private AppointmentStaffSupport appointmentStaffSupport;

    /**
     * 分页查询预约工单
     *
     * @param params       查询条件
     * @param managerScope 是否管理端全局视角
     * @return 工单分页VO
     */
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
        Map<String, String> staffNameMap = appointmentStaffSupport.loadStaffNameMap(result.getRecords());
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

    /**
     * 完结工单服务，并将关联主订单推进到已完成
     *
     * @param storeId     店铺ID
     * @param workOrderId 工单ID
     */
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
        orderLifecycleSupport.complete(order.getSn());
    }

    /**
     * 技师/店员认领工单
     *
     * @param storeId     店铺ID
     * @param userId      操作人ID
     * @param workOrderId 工单ID
     */
    @Transactional(rollbackFor = Exception.class)
    public void claimWorkOrder(String storeId, String userId, String workOrderId) {
        AppointmentWorkOrder workOrder = loadStoreWorkOrder(storeId, workOrderId);
        if (!AppointmentWorkOrderStatusEnum.PENDING_CLAIM.name().equals(workOrder.getServiceStatus())) {
            throw new ServiceException(ResultCode.APPOINTMENT_WORK_ORDER_STATUS_ERROR);
        }
        workOrder.setServiceStatus(AppointmentWorkOrderStatusEnum.CLAIMED.name());
        workOrderMapper.updateById(workOrder);
    }

    /**
     * 改派工单技师
     *
     * @param storeId     店铺ID
     * @param dto         改派参数
     * @param workOrderId 工单ID
     */
    @Transactional(rollbackFor = Exception.class)
    public void reassignWorkOrder(String storeId, AppointmentReassignDTO dto, String workOrderId) {
        appointmentStaffSupport.validateEnabledStaff(storeId, dto.getStaffId());
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

    /**
     * 统计店铺各工单状态数量
     *
     * @param storeId 店铺ID
     * @return 状态及数量字典
     */
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

    /**
     * 根据订单号加载工单
     *
     * @param orderSn 订单编号
     * @return 工单实体
     */
    public AppointmentWorkOrder loadWorkOrderByOrderSn(String orderSn) {
        AppointmentWorkOrder workOrder = workOrderMapper.selectOne(new LambdaQueryWrapper<AppointmentWorkOrder>()
                .eq(AppointmentWorkOrder::getOrderSn, orderSn)
                .eq(AppointmentWorkOrder::getDeleteFlag, false));
        if (workOrder == null) {
            throw new ServiceException(ResultCode.APPOINTMENT_WORK_ORDER_NOT_FOUND);
        }
        return workOrder;
    }

    /**
     * 根据店铺ID和工单ID加载工单
     *
     * @param storeId     店铺ID
     * @param workOrderId 工单ID
     * @return 工单实体
     */
    public AppointmentWorkOrder loadStoreWorkOrder(String storeId, String workOrderId) {
        AppointmentWorkOrder workOrder = workOrderMapper.selectById(workOrderId);
        if (workOrder == null || !storeId.equals(workOrder.getStoreId()) || Boolean.TRUE.equals(workOrder.getDeleteFlag())) {
            throw new ServiceException(ResultCode.APPOINTMENT_WORK_ORDER_NOT_FOUND);
        }
        return workOrder;
    }
}
