package cn.lili.modules.distribution.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.IdUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.distribution.entity.enums.DistributionRecruitFieldTypeEnum;
import cn.lili.modules.distribution.entity.enums.RecruitApplyModeEnum;
import cn.lili.modules.distribution.entity.enums.RecruitJoinConditionTypeEnum;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionRecruitFieldSetting;
import cn.lili.modules.system.entity.dto.DistributionRecruitSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * 分销招募设置读写（li_setting）
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Component
public class DistributionRecruitSettingSupport {

    public static final String SETTING_KEY = SettingEnum.DISTRIBUTION_RECRUIT_SETTING.name();

    private static final int MAX_RECRUIT_FIELDS = 5;

    @Autowired
    private SettingService settingService;

    /**
     * 读取招募设置，不存在时返回默认配置。
     */
    public DistributionRecruitSetting readSetting() {
        Setting setting = settingService.get(SETTING_KEY);
        if (setting == null || CharSequenceUtil.isBlank(setting.getSettingValue())) {
            return DistributionRecruitSetting.defaultSetting();
        }
        DistributionRecruitSetting value = JSONUtil.toBean(setting.getSettingValue(), DistributionRecruitSetting.class);
        if (value == null) {
            return DistributionRecruitSetting.defaultSetting();
        }
        // 补全默认值与字段排序
        normalize(value);
        return value;
    }

    /**
     * 保存招募设置（版本号自动递增）。
     *
     * @param incoming 待保存配置
     * @return 保存后的配置
     */
    public DistributionRecruitSetting saveSetting(DistributionRecruitSetting incoming) {
        validate(incoming);
        normalize(incoming);
        DistributionRecruitSetting existing = readSettingRaw();
        // 每次保存递增方案版本，便于前端感知配置变更
        int nextVersion = existing == null || existing.getPlanVersion() == null
                ? 1 : existing.getPlanVersion() + 1;
        incoming.setPlanVersion(nextVersion);

        Setting entity = settingService.getById(SETTING_KEY);
        if (entity == null) {
            entity = new Setting();
            entity.setId(SETTING_KEY);
        }
        entity.setSettingValue(JSONUtil.toJsonStr(incoming));
        settingService.saveUpdate(entity);
        return incoming;
    }

    /**
     * 校验招募设置业务规则。
     */
    public void validate(DistributionRecruitSetting setting) {
        if (setting == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        // AUTO 模式不收集申请表单，与运营后台 UI 一致
        if (RecruitApplyModeEnum.AUTO.name().equals(setting.getApplyMode())) {
            setting.setRequireApplyInfo(false);
        }
        if (!RecruitJoinConditionTypeEnum.CONDITIONAL.name().equals(setting.getJoinConditionType())) {
            // 无条件加入时跳过后续门槛校验
            return;
        }
        boolean requirePurchaseGoods = Boolean.TRUE.equals(setting.getRequirePurchaseGoods());
        boolean requireSelfPurchaseAmount = Boolean.TRUE.equals(setting.getRequireSelfPurchaseAmount());
        boolean requireConsumeCount = Boolean.TRUE.equals(setting.getRequireConsumeCount());
        if (!requirePurchaseGoods && !requireSelfPurchaseAmount && !requireConsumeCount) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请至少选择一个加入条件");
        }
        if (requirePurchaseGoods && !hasSelectedGoods(setting)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请选择指定商品");
        }
        if (requireSelfPurchaseAmount) {
            BigDecimal amount = setting.getMinSelfPurchaseAmount() == null
                    ? BigDecimal.ZERO : setting.getMinSelfPurchaseAmount();
            if (amount.compareTo(new BigDecimal("0.01")) < 0) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "自购金额最低设置为0.01元");
            }
        }
        if (requireConsumeCount) {
            int count = setting.getMinConsumeCount() == null ? 0 : setting.getMinConsumeCount();
            if (count < 1) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "消费笔数最低设置为1笔");
            }
        }
        // 开启申请表单时校验字段数量与格式
        if (Boolean.TRUE.equals(setting.getRequireApplyInfo())) {
            List<DistributionRecruitFieldSetting> fields = setting.getFields();
            if (fields == null || fields.isEmpty()) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "请至少添加一个申请字段");
            }
            if (fields.size() > MAX_RECRUIT_FIELDS) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "申请字段最多添加5个");
            }
            for (DistributionRecruitFieldSetting field : fields) {
                validateField(field);
            }
        }
    }

    /**
     * 归一化默认值与字段 ID、排序。
     */
    public void normalize(DistributionRecruitSetting setting) {
        if (setting == null) {
            return;
        }
        if (setting.getJoinConditionType() == null) {
            setting.setJoinConditionType(RecruitJoinConditionTypeEnum.NONE.name());
        }
        if (setting.getOnlineEntryVisible() == null) {
            setting.setOnlineEntryVisible(true);
        }
        if (RecruitApplyModeEnum.AUTO.name().equals(setting.getApplyMode())) {
            setting.setRequireApplyInfo(false);
        }
        if (setting.getRequireApplyInfo() == null) {
            setting.setRequireApplyInfo(false);
        }
        if (!RecruitJoinConditionTypeEnum.CONDITIONAL.name().equals(setting.getJoinConditionType())) {
            // 非条件加入时清空门槛相关配置，避免脏数据残留
            setting.setRequirePurchaseGoods(false);
            setting.setRequireSelfPurchaseAmount(false);
            setting.setRequireConsumeCount(false);
            setting.setRecruitGoods(Collections.emptyList());
        }
        if (!Boolean.TRUE.equals(setting.getRequireApplyInfo())) {
            setting.setFields(Collections.emptyList());
        } else if (setting.getFields() != null) {
            // 为无 ID 字段生成主键并统一排序
            int sort = 0;
            for (DistributionRecruitFieldSetting field : setting.getFields()) {
                if (field.getId() == null || field.getId().isBlank()) {
                    field.setId(IdUtil.simpleUUID());
                }
                field.setSortOrder(sort++);
                if (field.getRequired() == null) {
                    field.setRequired(true);
                }
            }
        }
        if (setting.getRecruitGoods() == null) {
            setting.setRecruitGoods(new ArrayList<>());
        }
        if (!Boolean.TRUE.equals(setting.getRequirePurchaseGoods())) {
            setting.setRecruitGoods(Collections.emptyList());
        }
        if (CharSequenceUtil.isNotBlank(setting.getPosterBackground())) {
            // 上传背景图即视为启用招募海报
            setting.setPosterEnabled(true);
        }
    }

    /**
     * 按字段 ID 查找配置。
     */
    public DistributionRecruitFieldSetting findField(DistributionRecruitSetting setting, String fieldId) {
        if (setting == null || fieldId == null || fieldId.isBlank() || setting.getFields() == null) {
            return null;
        }
        return setting.getFields().stream()
                .filter(field -> fieldId.equals(field.getId()))
                .findFirst()
                .orElse(null);
    }

    private DistributionRecruitSetting readSettingRaw() {
        Setting setting = settingService.get(SETTING_KEY);
        if (setting == null || CharSequenceUtil.isBlank(setting.getSettingValue())) {
            return null;
        }
        return JSONUtil.toBean(setting.getSettingValue(), DistributionRecruitSetting.class);
    }

    private void validateField(DistributionRecruitFieldSetting field) {
        if (field.getFieldName() == null || field.getFieldName().isBlank()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "字段名称不能为空");
        }
        if (field.getFieldType() == null || field.getFieldType().isBlank()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "字段格式不能为空");
        }
        try {
            DistributionRecruitFieldTypeEnum.valueOf(field.getFieldType());
        } catch (IllegalArgumentException ex) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "字段格式不支持");
        }
    }

    private boolean hasSelectedGoods(DistributionRecruitSetting setting) {
        if (setting.getRecruitGoods() == null || setting.getRecruitGoods().isEmpty()) {
            return false;
        }
        return setting.getRecruitGoods().stream()
                .anyMatch(item -> item != null
                        && item.getGoodsId() != null
                        && !item.getGoodsId().isBlank());
    }
}
