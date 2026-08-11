package cn.lili.modules.distribution.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionPromotionPosterSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 分销平台海报设置
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Component
public class DistributionPromotionPosterSettingSupport {

    private static final String DEFAULT_SLOGAN = "发现好物，邀请你一起逛逛";
    private static final String DEFAULT_TEXT_COLOR = "#1f2a44";

    @Autowired
    private SettingService settingService;

    /**
     * 读取平台海报设置（兼容历史主推商品配置）。
     */
    public DistributionPromotionPosterSetting readSetting() {
        Setting setting = settingService.get(SettingEnum.DISTRIBUTION_PROMOTION_POSTER_SETTING.name());
        if (setting != null && CharSequenceUtil.isNotBlank(setting.getSettingValue())) {
            DistributionPromotionPosterSetting value = JSONUtil.toBean(
                    setting.getSettingValue(), DistributionPromotionPosterSetting.class);
            if (value != null) {
                normalize(value);
                return value;
            }
        }
        // 新 key 无数据时，从旧版主推商品配置迁移海报字段
        return readLegacySetting();
    }

    /**
     * 保存平台海报设置。
     *
     * @param setting 海报设置
     */
    public void saveSetting(DistributionPromotionPosterSetting setting) {
        normalize(setting);
        validate(setting);
        Setting entity = settingService.getById(SettingEnum.DISTRIBUTION_PROMOTION_POSTER_SETTING.name());
        if (entity == null) {
            entity = new Setting();
            entity.setId(SettingEnum.DISTRIBUTION_PROMOTION_POSTER_SETTING.name());
        }
        entity.setSettingValue(JSONUtil.toJsonStr(setting));
        settingService.saveUpdate(entity);
    }

    /**
     * 归一化平台海报设置默认值。
     *
     * @param setting 海报设置
     */
    public void normalize(DistributionPromotionPosterSetting setting) {
        if (setting == null) {
            return;
        }
        if (setting.getMemberInfoVisible() == null) {
            setting.setMemberInfoVisible(Boolean.TRUE);
        }
        if (CharSequenceUtil.isBlank(setting.getSlogan())) {
            setting.setSlogan(DEFAULT_SLOGAN);
        } else {
            setting.setSlogan(setting.getSlogan().trim());
        }
        if (CharSequenceUtil.isBlank(setting.getTextColor())) {
            setting.setTextColor(DEFAULT_TEXT_COLOR);
        } else {
            setting.setTextColor(setting.getTextColor().trim());
        }
        if (setting.getBackgroundImage() != null) {
            setting.setBackgroundImage(setting.getBackgroundImage().trim());
        }
    }

    /**
     * 校验平台海报设置参数。
     *
     * @param setting 海报设置
     */
    public void validate(DistributionPromotionPosterSetting setting) {
        if (setting == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        if (setting.getSlogan().length() > 200) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "推荐语不能超过200字");
        }
        if (!isValidHexColor(setting.getTextColor())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "文字颜色格式不正确");
        }
    }

    /**
     * 从旧版主推商品配置中读取平台海报设置。
     *
     * @return 平台海报设置
     */
    private DistributionPromotionPosterSetting readLegacySetting() {
        DistributionPromotionPosterSetting legacy = new DistributionPromotionPosterSetting();
        Setting featuredSetting = settingService.get(SettingEnum.DISTRIBUTION_FEATURED_GOODS_SETTING.name());
        if (featuredSetting != null && CharSequenceUtil.isNotBlank(featuredSetting.getSettingValue())) {
            cn.hutool.json.JSONObject json = JSONUtil.parseObj(featuredSetting.getSettingValue());
            legacy.setBackgroundImage(json.getStr("promotionPosterBackground"));
            if (CharSequenceUtil.isNotBlank(json.getStr("promotionPosterSlogan"))) {
                legacy.setSlogan(json.getStr("promotionPosterSlogan"));
            }
        }
        // 补全默认推荐语与文字颜色
        normalize(legacy);
        return legacy;
    }

    /**
     * 校验是否为合法的十六进制颜色值（#RGB 或 #RRGGBB）。
     *
     * @param color 颜色字符串
     * @return 是否合法
     */
    private boolean isValidHexColor(String color) {
        if (CharSequenceUtil.isBlank(color)) {
            return false;
        }
        return color.matches("^#([0-9a-fA-F]{3}|[0-9a-fA-F]{6})$");
    }
}
