package cn.lili.modules.appointment.support;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.appointment.entity.dos.AppointmentOrderExt;
import cn.lili.modules.appointment.entity.dos.AppointmentVerificationCode;
import cn.lili.modules.appointment.entity.dos.AppointmentWorkOrder;
import cn.lili.modules.appointment.entity.dto.AppointmentVerifyCodeDTO;
import cn.lili.modules.appointment.entity.enums.AppointmentServiceModeEnum;
import cn.lili.modules.appointment.entity.enums.AppointmentVerificationCodeStatusEnum;
import cn.lili.modules.appointment.entity.enums.AppointmentWorkOrderStatusEnum;
import cn.lili.modules.appointment.entity.vo.AppointmentBuyerOrderDetailVO;
import cn.lili.modules.appointment.entity.vo.AppointmentVerificationCodeVO;
import cn.lili.modules.appointment.entity.vo.AppointmentVerificationOrderVO;
import cn.lili.modules.appointment.entity.vo.AppointmentVerificationResultVO;
import cn.lili.modules.appointment.mapper.AppointmentOrderExtMapper;
import cn.lili.modules.appointment.mapper.AppointmentVerificationCodeMapper;
import cn.lili.modules.appointment.mapper.AppointmentWorkOrderMapper;
import cn.lili.modules.appointment.util.AppointmentSnUtil;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 预约核销码生命周期与核验业务支持组件
 * <p>
 * 负责核销码生成、核销码真伪校验、多码多次核销、状态推进与作废。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Component
public class AppointmentVerificationSupport {

    @Autowired
    private AppointmentVerificationCodeMapper verificationCodeMapper;

    @Autowired
    private AppointmentOrderExtMapper orderExtMapper;

    @Autowired
    private AppointmentWorkOrderMapper workOrderMapper;

    @Autowired
    private OrderMapper orderMapper;

    @Autowired
    private OrderService orderService;

    @Autowired
    private AppointmentStaffSupport appointmentStaffSupport;

    @Autowired
    private AppointmentWorkOrderSupport appointmentWorkOrderSupport;

    /**
     * 根据核销码查询关联订单详情
     *
     * @param storeId 店铺ID
     * @param code    核销码
     * @return 核销订单详情VO
     */
    public AppointmentVerificationOrderVO getOrderByVerificationCode(String storeId, String code) {
        AppointmentVerificationCode verificationCode = loadVerificationCode(storeId, code);
        return buildVerificationOrderVO(verificationCode.getOrderSn(), storeId);
    }

    /**
     * 根据订单号查询核销详情
     *
     * @param storeId 店铺ID
     * @param orderSn 订单编号
     * @return 核销订单详情VO
     */
    public AppointmentVerificationOrderVO getVerificationByOrderSn(String storeId, String orderSn) {
        Order order = orderMapper.selectOne(new LambdaQueryWrapper<Order>()
                .eq(Order::getSn, orderSn)
                .eq(Order::getStoreId, storeId));
        if (order == null || !OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        return buildVerificationOrderVO(orderSn, storeId);
    }

    /**
     * 买家端查询预约订单详情（包含预约时间、地址、表单及核销码）
     *
     * @param memberId 会员ID
     * @param orderSn  订单编号
     * @return 买家端预约详情VO
     */
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

    /**
     * 根据订单编号读取买家预约详情（系统/管理端用）
     *
     * @param orderSn 订单编号
     * @return 详情VO
     */
    public AppointmentBuyerOrderDetailVO getOrderExtDetail(String orderSn) {
        Order order = orderMapper.selectOne(new LambdaQueryWrapper<Order>().eq(Order::getSn, orderSn));
        if (order == null || !OrderTypeEnum.APPOINTMENT.name().equals(order.getOrderType())) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        return getBuyerOrderDetail(order.getMemberId(), orderSn);
    }

    /**
     * 校验店铺是否存在该核销码
     *
     * @param storeId 店铺ID
     * @param code    核销码
     * @return 是否存在
     */
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

    /**
     * 查找核销码实体
     *
     * @param storeId 店铺ID
     * @param code    核销码
     * @return 核销码实体
     */
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

    /**
     * 门店操作员快速核销（不指定技师）
     *
     * @param storeId 店铺ID
     * @param userId  操作员ID
     * @param code    核销码
     * @return 核销结果VO
     */
    @Transactional(rollbackFor = Exception.class)
    public AppointmentVerificationResultVO verifyCodeByOperator(String storeId, String userId, String code) {
        return doVerifyCode(storeId, userId, code, null);
    }

    /**
     * 指定技师或核销参数核验核销码
     *
     * @param storeId 店铺ID
     * @param userId  操作员ID
     * @param dto     核验DTO
     * @return 核销结果VO
     */
    @Transactional(rollbackFor = Exception.class)
    public AppointmentVerificationResultVO verifyCode(String storeId, String userId, AppointmentVerifyCodeDTO dto) {
        if (dto == null || CharSequenceUtil.isBlank(dto.getCode())) {
            throw new ServiceException(ResultCode.APPOINTMENT_CODE_NOT_FOUND);
        }
        return doVerifyCode(storeId, userId, dto.getCode(), dto.getStaffId());
    }

    /**
     * 统一核心核销执行逻辑
     *
     * @param storeId 店铺ID
     * @param userId  操作人ID
     * @param code    核销码
     * @param staffId 关联技师ID（可为空）
     * @return 核销结果
     */
    public AppointmentVerificationResultVO doVerifyCode(String storeId, String userId, String code, String staffId) {
        boolean withStaff = CharSequenceUtil.isNotBlank(staffId);
        if (withStaff) {
            appointmentStaffSupport.validateEnabledStaff(storeId, staffId);
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
        AppointmentWorkOrder workOrder = appointmentWorkOrderSupport.loadWorkOrderByOrderSn(verificationCode.getOrderSn());

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

    /**
     * 生成店铺唯一的 8 位纯数字预约核销码
     *
     * @param storeId   店铺ID
     * @param usedCodes 本批次已占用的核销码集合
     * @return 唯一核销码
     */
    public String generateUniqueCode(String storeId, Set<String> usedCodes) {
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

    /**
     * 预约订单履约完成后推进主订单至待核销状态，并发送状态变更通知
     *
     * @param orderSn 订单编号
     */
    public void transitionOrderToTake(String orderSn) {
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

    /**
     * 批量作废订单下所有未核销的核销码
     *
     * @param orderSn 订单编号
     */
    public void voidVerificationCodes(String orderSn) {
        verificationCodeMapper.update(null, new LambdaUpdateWrapper<AppointmentVerificationCode>()
                .eq(AppointmentVerificationCode::getOrderSn, orderSn)
                .eq(AppointmentVerificationCode::getStatus, AppointmentVerificationCodeStatusEnum.UNUSED.name())
                .set(AppointmentVerificationCode::getStatus, AppointmentVerificationCodeStatusEnum.VOIDED.name()));
    }

    /**
     * 根据店铺与码读取核销码记录
     */
    public AppointmentVerificationCode loadVerificationCode(String storeId, String code) {
        if (CharSequenceUtil.isBlank(code)) {
            throw new ServiceException(ResultCode.APPOINTMENT_CODE_NOT_FOUND);
        }
        AppointmentVerificationCode verificationCode = verificationCodeMapper.selectOne(
                new LambdaQueryWrapper<AppointmentVerificationCode>()
                        .eq(AppointmentVerificationCode::getStoreId, storeId)
                        .eq(AppointmentVerificationCode::getCode, code.trim())
                        .eq(AppointmentVerificationCode::getDeleteFlag, false));
        if (verificationCode == null) {
            throw new ServiceException(ResultCode.APPOINTMENT_CODE_NOT_FOUND);
        }
        return verificationCode;
    }

    /**
     * 校验核销时的主订单合法性
     */
    public void validateAppointmentOrderForVerify(Order order, String storeId) {
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

    /**
     * 约束同一订单多次核销必须由同一技师完成
     */
    public void enforceSameStaffForOrder(String orderSn, String staffId) {
        List<AppointmentVerificationCode> usedCodes = verificationCodeMapper.selectList(
                new LambdaQueryWrapper<AppointmentVerificationCode>()
                        .eq(AppointmentVerificationCode::getOrderSn, orderSn)
                        .eq(AppointmentVerificationCode::getStatus, AppointmentVerificationCodeStatusEnum.USED.name())
                        .eq(AppointmentVerificationCode::getDeleteFlag, false));
        for (AppointmentVerificationCode used : usedCodes) {
            if (CharSequenceUtil.isNotBlank(used.getStaffId()) && !staffId.equals(used.getStaffId())) {
                throw new ServiceException(ResultCode.APPOINTMENT_CODE_STAFF_MISMATCH);
            }
        }
    }

    /**
     * 查询订单关联的所有核销码
     */
    public List<AppointmentVerificationCode> listOrderVerificationCodes(String orderSn) {
        return verificationCodeMapper.selectList(new LambdaQueryWrapper<AppointmentVerificationCode>()
                .eq(AppointmentVerificationCode::getOrderSn, orderSn)
                .eq(AppointmentVerificationCode::getDeleteFlag, false)
                .orderByAsc(AppointmentVerificationCode::getSeqNo));
    }

    /**
     * 计算已核销的码数量
     */
    public int countUsedCodes(List<AppointmentVerificationCode> codes) {
        if (CollUtil.isEmpty(codes)) {
            return 0;
        }
        return (int) codes.stream()
                .filter(c -> AppointmentVerificationCodeStatusEnum.USED.name().equals(c.getStatus()))
                .count();
    }

    /**
     * 读取订单预约扩展
     */
    public AppointmentOrderExt loadOrderExt(String orderSn) {
        AppointmentOrderExt ext = orderExtMapper.selectOne(new LambdaQueryWrapper<AppointmentOrderExt>()
                .eq(AppointmentOrderExt::getOrderSn, orderSn)
                .eq(AppointmentOrderExt::getDeleteFlag, false));
        if (ext == null) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        return ext;
    }

    /**
     * 组装核销订单视图VO
     */
    public AppointmentVerificationOrderVO buildVerificationOrderVO(String orderSn, String storeId) {
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
        Map<String, String> staffNameMap = appointmentStaffSupport.loadStaffNameIds(orderCodes.stream()
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
}
