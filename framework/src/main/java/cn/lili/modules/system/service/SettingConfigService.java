package cn.lili.modules.system.service;

import cn.lili.modules.system.entity.enums.SettingEnum;

/**
 * 系统配置应用接口，统一提供带业务校验的保存和带默认值的读取，不承担底层通用持久化。
 * @author Bulbasaur
 * @since 2026-11-05
 */
public interface SettingConfigService {
    /**
     * 按类型转换、过滤并校验配置后保存，专用业务规则由对应服务处理。
     * @param settingEnum 配置类型，对应 li_setting.id
     * @param configValue 配置 JSON
     */
    void saveConfig(SettingEnum settingEnum, String configValue);

    /**
     * 读取对应配置 DTO，未配置时返回默认值，并兼容历史空值。
     * @param settingEnum 配置类型，对应 li_setting.id
     * @return 配置 DTO
     */
    Object getConfig(SettingEnum settingEnum);
}
