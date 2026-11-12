package cn.lili.modules.appointment.support;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.appointment.entity.dos.AppointmentStoreConfig;
import cn.lili.modules.appointment.entity.dos.AppointmentSystemForm;
import cn.lili.modules.appointment.entity.dto.AppointmentStoreConfigDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentSystemFormDTO;
import cn.lili.modules.appointment.entity.enums.AppointmentServiceModeEnum;
import cn.lili.modules.appointment.mapper.AppointmentStoreConfigMapper;
import cn.lili.modules.appointment.mapper.AppointmentSystemFormMapper;
import cn.lili.modules.appointment.util.AppointmentGeoFence;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.AppointmentOrderSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * 预约服务配置与表单管理业务支持组件
 * <p>
 * 负责门店到店/上门配置维护、电子表单定义与全域预约订单系统设置存储。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Component
public class AppointmentConfigSupport {

    @Autowired
    private AppointmentStoreConfigMapper storeConfigMapper;

    @Autowired
    private AppointmentSystemFormMapper systemFormMapper;

    @Autowired
    private SettingService settingService;

    /**
     * 查询店铺服务配置列表
     *
     * @param storeId 店铺ID
     * @return 配置列表
     */
    public List<AppointmentStoreConfig> listStoreConfig(String storeId) {
        return storeConfigMapper.selectList(new LambdaQueryWrapper<AppointmentStoreConfig>()
                .eq(AppointmentStoreConfig::getStoreId, storeId)
                .eq(AppointmentStoreConfig::getDeleteFlag, false));
    }

    /**
     * 保存或更新店铺服务模式配置
     *
     * @param storeId 店铺ID
     * @param dto     配置参数
     */
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

    /**
     * 获取门店到店服务设置视图
     *
     * @param storeId 店铺ID
     * @return 扁平键值对设置
     */
    public Map<String, Object> getInStoreSettings(String storeId) {
        AppointmentStoreConfig config = loadStoreConfig(storeId, AppointmentServiceModeEnum.IN_STORE.name());
        Map<String, Object> form = new LinkedHashMap<>();
        form.put("userBooking", true);
        form.put("workOrder", config == null || !Boolean.FALSE.equals(config.getWorkOrderVisible()));
        form.put("endService", true);
        return form;
    }

    /**
     * 保存门店到店服务设置
     *
     * @param storeId 店铺ID
     * @param form    表单参数
     */
    @Transactional(rollbackFor = Exception.class)
    public void saveInStoreSettings(String storeId, Map<String, Object> form) {
        AppointmentStoreConfigDTO dto = new AppointmentStoreConfigDTO();
        dto.setServiceMode(AppointmentServiceModeEnum.IN_STORE.name());
        dto.setWorkOrderVisible(readBoolean(form, "workOrder", true));
        saveStoreConfig(storeId, dto);
    }

    /**
     * 获取门面上门服务设置视图
     *
     * @param storeId 店铺ID
     * @return 扁平键值对设置
     */
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

    /**
     * 保存门面上门服务设置
     *
     * @param storeId 店铺ID
     * @param form    表单参数
     */
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

    /**
     * 加载指定模式的店铺配置
     *
     * @param storeId     店铺ID
     * @param serviceMode 服务模式
     * @return 配置实体
     */
    public AppointmentStoreConfig loadStoreConfig(String storeId, String serviceMode) {
        return storeConfigMapper.selectOne(new LambdaQueryWrapper<AppointmentStoreConfig>()
                .eq(AppointmentStoreConfig::getStoreId, storeId)
                .eq(AppointmentStoreConfig::getServiceMode, serviceMode)
                .eq(AppointmentStoreConfig::getDeleteFlag, false));
    }

    /**
     * 分页查询系统自定义表单列表
     *
     * @param storeId    店铺ID
     * @param pageNumber 页码
     * @param pageSize   每页数量
     * @param formName   表单名称
     * @param status     状态
     * @return 表单分页
     */
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

    /**
     * 获取表单详情
     *
     * @param storeId 店铺ID
     * @param formId  表单ID
     * @return 表单实体
     */
    public AppointmentSystemForm getSystemForm(String storeId, String formId) {
        AppointmentSystemForm form = systemFormMapper.selectById(formId);
        if (form == null || !storeId.equals(form.getStoreId()) || Boolean.TRUE.equals(form.getDeleteFlag())) {
            throw new ServiceException(ResultCode.APPOINTMENT_SYSTEM_FORM_NOT_FOUND);
        }
        return form;
    }

    /**
     * 保存或更新自定义表单
     *
     * @param storeId 店铺ID
     * @param dto     表单DTO
     * @return 表单实体
     */
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

    /**
     * 删除自定义表单（逻辑删除）
     *
     * @param storeId 店铺ID
     * @param formId  表单ID
     */
    @Transactional(rollbackFor = Exception.class)
    public void deleteSystemForm(String storeId, String formId) {
        AppointmentSystemForm form = systemFormMapper.selectById(formId);
        if (form == null || !storeId.equals(form.getStoreId())) {
            throw new ServiceException(ResultCode.APPOINTMENT_SYSTEM_FORM_NOT_FOUND);
        }
        form.setDeleteFlag(true);
        systemFormMapper.updateById(form);
    }

    /**
     * 查询店铺已启用的自定义表单
     *
     * @param storeId 店铺ID
     * @return 表单列表
     */
    public List<AppointmentSystemForm> listEnabledSystemForms(String storeId) {
        return systemFormMapper.selectList(new LambdaQueryWrapper<AppointmentSystemForm>()
                .eq(AppointmentSystemForm::getStoreId, storeId)
                .eq(AppointmentSystemForm::getStatus, "ENABLED")
                .eq(AppointmentSystemForm::getDeleteFlag, false)
                .orderByDesc(AppointmentSystemForm::getCreateTime));
    }

    /**
     * 读取全局预约订单配置
     *
     * @return 预约配置DTO
     */
    public AppointmentOrderSetting getAppointmentOrderSetting() {
        Setting setting = settingService.get(SettingEnum.APPOINTMENT_ORDER_SETTING.name());
        if (setting == null || CharSequenceUtil.isBlank(setting.getSettingValue())) {
            return new AppointmentOrderSetting();
        }
        return JSONUtil.toBean(setting.getSettingValue(), AppointmentOrderSetting.class);
    }

    /**
     * 保存全局预约订单配置
     *
     * @param setting 预约配置
     */
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

    /**
     * 从字典安全读取布尔值
     *
     * @param form         表单Map
     * @param key          字段名
     * @param defaultValue 缺省默认值
     * @return 布尔结果
     */
    public boolean readBoolean(Map<String, Object> form, String key, boolean defaultValue) {
        if (form == null || !form.containsKey(key) || form.get(key) == null) {
            return defaultValue;
        }
        Object value = form.get(key);
        if (value instanceof Boolean) {
            return (Boolean) value;
        }
        return Boolean.parseBoolean(String.valueOf(value));
    }
}
