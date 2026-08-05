package cn.lili.modules.distribution.support;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 分销基础设置读取与归一化。
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Component
public class DistributionSettingSupport {

    @Autowired
    private SettingService settingService;

    /**
     * 读取分销基础设置。
     *
     * @return 分销设置
     */
    public DistributionSetting getSetting() {
        Setting setting = settingService.get(SettingEnum.DISTRIBUTION_SETTING.name());
        DistributionSetting distributionSetting = setting == null || setting.getSettingValue() == null
                ? DistributionSetting.defaultSetting()
                : JSONUtil.toBean(setting.getSettingValue(), DistributionSetting.class);
        distributionSetting.applyDefaults();
        return distributionSetting;
    }

    /**
     * 保存并校验分销基础设置。
     *
     * @param setting 分销设置
     */
    public void saveSetting(DistributionSetting setting) {
        if (setting == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        normalize(setting);
        Setting entity = settingService.getById(SettingEnum.DISTRIBUTION_SETTING.name());
        if (entity == null) {
            entity = new Setting();
            entity.setId(SettingEnum.DISTRIBUTION_SETTING.name());
        }
        entity.setSettingValue(JSONUtil.toJsonStr(setting));
        settingService.saveUpdate(entity);
    }

    /**
     * 归一化并校验分销基础设置字段。
     *
     * @param setting 分销设置
     */
    public void normalize(DistributionSetting setting) {
        if (setting == null) {
            return;
        }
        setting.applyDefaults();
        if (setting.getDistributorName().length() > 20) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "分销员名称不能超过20个字符");
        }
    }
}
