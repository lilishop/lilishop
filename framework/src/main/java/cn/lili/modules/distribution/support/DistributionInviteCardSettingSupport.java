package cn.lili.modules.distribution.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionInviteCardSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 分销邀请卡设置
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Component
public class DistributionInviteCardSettingSupport {

    private static final String DEFAULT_SLOGAN = "邀请您成为我的下级分销员";
    private static final String DEFAULT_TEXT_COLOR = "#1f2a44";

    @Autowired
    private SettingService settingService;

    /**
     * 读取邀请卡设置。
     */
    public DistributionInviteCardSetting readSetting() {
        Setting setting = settingService.get(SettingEnum.DISTRIBUTION_INVITE_CARD_SETTING.name());
        if (setting != null && CharSequenceUtil.isNotBlank(setting.getSettingValue())) {
            DistributionInviteCardSetting value = JSONUtil.toBean(
                    setting.getSettingValue(), DistributionInviteCardSetting.class);
            if (value != null) {
                normalize(value);
                return value;
            }
        }
        // 无配置时返回带默认文案的对象
        return defaultSetting();
    }

    /**
     * 保存邀请卡设置。
     *
     * @param setting 邀请卡设置
     */
    public void saveSetting(DistributionInviteCardSetting setting) {
        normalize(setting);
        validate(setting);
        Setting entity = settingService.getById(SettingEnum.DISTRIBUTION_INVITE_CARD_SETTING.name());
        if (entity == null) {
            entity = new Setting();
            entity.setId(SettingEnum.DISTRIBUTION_INVITE_CARD_SETTING.name());
        }
        entity.setSettingValue(JSONUtil.toJsonStr(setting));
        settingService.saveUpdate(entity);
    }

    /**
     * 归一化邀请卡设置默认值。
     *
     * @param setting 邀请卡设置
     */
    public void normalize(DistributionInviteCardSetting setting) {
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
     * 校验邀请卡设置参数。
     *
     * @param setting 邀请卡设置
     */
    public void validate(DistributionInviteCardSetting setting) {
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
     * 构建默认邀请卡设置。
     *
     * @return 默认邀请卡设置
     */
    private DistributionInviteCardSetting defaultSetting() {
        DistributionInviteCardSetting setting = new DistributionInviteCardSetting();
        normalize(setting);
        return setting;
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
