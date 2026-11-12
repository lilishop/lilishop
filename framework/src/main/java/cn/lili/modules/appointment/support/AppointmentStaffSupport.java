package cn.lili.modules.appointment.support;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.appointment.entity.dos.AppointmentServiceStaff;
import cn.lili.modules.appointment.entity.dos.AppointmentWorkOrder;
import cn.lili.modules.appointment.entity.dto.AppointmentServiceStaffDTO;
import cn.lili.modules.appointment.entity.dto.search.AppointmentWorkOrderSearchParams;
import cn.lili.modules.appointment.mapper.AppointmentServiceStaffMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 预约服务技师与员工业务支持组件
 * <p>
 * 负责服务人员增删改查、在职状态校验以及姓名映射缓存装配。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Component
public class AppointmentStaffSupport {

    @Autowired
    private AppointmentServiceStaffMapper serviceStaffMapper;

    /**
     * 分页查询服务技师列表
     *
     * @param storeId 店铺ID
     * @param params  查询参数
     * @return 分页数据
     */
    public IPage<AppointmentServiceStaff> pageStaff(String storeId, AppointmentWorkOrderSearchParams params) {
        Page<AppointmentServiceStaff> page = new Page<>(params.getPageNumber(), params.getPageSize());
        return serviceStaffMapper.selectPage(page, new LambdaQueryWrapper<AppointmentServiceStaff>()
                .eq(AppointmentServiceStaff::getStoreId, storeId)
                .eq(AppointmentServiceStaff::getDeleteFlag, false)
                .orderByAsc(AppointmentServiceStaff::getSortOrder)
                .orderByDesc(AppointmentServiceStaff::getCreateTime));
    }

    /**
     * 保存或更新服务技师信息
     *
     * @param storeId 店铺ID
     * @param dto     技师信息DTO
     * @return 保存后的实体
     */
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

    /**
     * 根据店员用户ID查询技师
     *
     * @param storeId     店铺ID
     * @param clerkUserId 店员用户ID
     * @return 技师实体
     */
    public AppointmentServiceStaff findStaffByClerk(String storeId, String clerkUserId) {
        return serviceStaffMapper.selectOne(new LambdaQueryWrapper<AppointmentServiceStaff>()
                .eq(AppointmentServiceStaff::getStoreId, storeId)
                .eq(AppointmentServiceStaff::getClerkUserId, clerkUserId)
                .last("LIMIT 1"));
    }

    /**
     * 换绑店员时，清掉已删除的同店员记录，避免唯一约束挡住更新
     *
     * @param occupied  已占用店员记录
     * @param currentId 当前技师ID
     */
    public void releaseDeletedClerk(AppointmentServiceStaff occupied, String currentId) {
        if (occupied == null || occupied.getId().equals(currentId)) {
            return;
        }
        if (Boolean.TRUE.equals(occupied.getDeleteFlag())) {
            serviceStaffMapper.deleteById(occupied.getId());
            return;
        }
        throw new ServiceException(ResultCode.APPOINTMENT_STAFF_EXISTS);
    }

    /**
     * 删除技师（逻辑删除）
     *
     * @param storeId 店铺ID
     * @param staffId 技师ID
     */
    @Transactional(rollbackFor = Exception.class)
    public void deleteStaff(String storeId, String staffId) {
        AppointmentServiceStaff staff = serviceStaffMapper.selectById(staffId);
        if (staff == null || !storeId.equals(staff.getStoreId())) {
            throw new ServiceException(ResultCode.APPOINTMENT_STAFF_NOT_FOUND);
        }
        staff.setDeleteFlag(true);
        serviceStaffMapper.updateById(staff);
    }

    /**
     * 查询店铺启用的技师列表
     *
     * @param storeId 店铺ID
     * @return 技师列表
     */
    public List<AppointmentServiceStaff> listEnabledStaff(String storeId) {
        return serviceStaffMapper.selectList(new LambdaQueryWrapper<AppointmentServiceStaff>()
                .eq(AppointmentServiceStaff::getStoreId, storeId)
                .eq(AppointmentServiceStaff::getStatus, "ENABLED")
                .eq(AppointmentServiceStaff::getDeleteFlag, false)
                .orderByAsc(AppointmentServiceStaff::getSortOrder)
                .orderByDesc(AppointmentServiceStaff::getCreateTime));
    }

    /**
     * 校验技师有效性且必须处于启用状态
     *
     * @param storeId 店铺ID
     * @param staffId 技师ID
     * @return 校验通过的技师实体
     */
    public AppointmentServiceStaff validateEnabledStaff(String storeId, String staffId) {
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

    /**
     * 批量加载工单关联的技师姓名映射
     *
     * @param records 工单列表
     * @return staffId -> staffName
     */
    public Map<String, String> loadStaffNameMap(Collection<AppointmentWorkOrder> records) {
        if (CollUtil.isEmpty(records)) {
            return Collections.emptyMap();
        }
        Set<String> staffIds = records.stream()
                .map(AppointmentWorkOrder::getStaffId)
                .filter(CharSequenceUtil::isNotBlank)
                .collect(Collectors.toSet());
        return loadStaffNameIds(staffIds);
    }

    /**
     * 批量根据技师ID集合加载技师姓名映射
     *
     * @param staffIds 技师ID集合
     * @return staffId -> staffName
     */
    public Map<String, String> loadStaffNameIds(Collection<String> staffIds) {
        if (CollUtil.isEmpty(staffIds)) {
            return Collections.emptyMap();
        }
        return serviceStaffMapper.selectBatchIds(staffIds).stream()
                .collect(Collectors.toMap(
                        AppointmentServiceStaff::getId,
                        AppointmentServiceStaff::getStaffName,
                        (a, b) -> a));
    }
}
