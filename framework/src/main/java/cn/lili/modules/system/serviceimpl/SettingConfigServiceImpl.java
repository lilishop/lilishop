package cn.lili.modules.system.serviceimpl;

import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.distribution.support.DistributionSettingSupport;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.*;
import cn.lili.modules.system.entity.dto.connect.*;
import cn.lili.modules.system.entity.dto.payment.*;
import cn.lili.modules.system.entity.dto.payment.dto.PaymentSupportForm;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingConfigService;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wxchannels.entity.dto.WxChannelsSetting;
import jakarta.validation.Validator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import java.util.Collections;

/**
 * 系统配置应用服务，统一处理配置读取、类型转换、业务校验和保存。
 * 底层 SettingService 负责配置持久化及缓存失效，本服务协调分销等专用业务规则，避免循环依赖。
 * @author Bulbasaur
 * @since 2026-11-05
 */
@Service
public class SettingConfigServiceImpl implements SettingConfigService {
    @Autowired
    private SettingService settingService;
    @Autowired
    private Validator validator;
    @Autowired
    private Cache<String> cache;
    @Autowired
    private DistributionSettingSupport distributionSettingSupport;

    /**
     * 保存前统一执行配置转换及校验；按配置类型执行专用业务规则，复用现有存储与缓存失效。
     * @param settingEnum 配置类型，对应 li_setting.id
     * @param configValue 提交的配置 JSON
     */
    @Override
    public void saveConfig(SettingEnum settingEnum, String configValue) {

        //获取系统配置
        Setting setting = settingService.getById(settingEnum.name());
        if (setting == null) {
            setting = new Setting();
            setting.setId(settingEnum.name());
        }
        //特殊配置过滤
        configValue = filter(settingEnum, configValue);

        setting.setSettingValue(configValue);
        settingService.saveUpdate(setting);
    }

    /**
     * 对配置进行过滤
     *
     * @param settingEnum 配置类型
     * @param configValue 提交的配置 JSON
     * @return 已完成业务规则处理的配置 JSON
     */
    private String filter(SettingEnum settingEnum, String configValue) {
        if (settingEnum.equals(SettingEnum.POINT_SETTING)) {
            PointSetting pointSetting = JSONUtil.toBean(configValue, PointSetting.class);
            if (pointSetting.getPointSettingItems() != null && pointSetting.getPointSettingItems().size() > 0) {
                Collections.sort(pointSetting.getPointSettingItems());
                if (pointSetting.getPointSettingItems().size() > 4) {
                    pointSetting.setPointSettingItems(pointSetting.getPointSettingItems().subList(0, 4));
                }
            }
            configValue = JSONUtil.toJsonStr(pointSetting);
        }
        if (settingEnum.equals(SettingEnum.DISTRIBUTION_SETTING)) {
            DistributionSetting distributionSetting = JSONUtil.toBean(configValue, DistributionSetting.class);
            distributionSettingSupport.normalize(distributionSetting);
            configValue = JSONUtil.toJsonStr(distributionSetting);
        }
        if (settingEnum.equals(SettingEnum.WITHDRAWAL_SETTING)) {
            configValue = validatedSetting(configValue, WithdrawalSetting.class);
        }
        if (settingEnum.equals(SettingEnum.WECHAT_OFFICIAL_ACCOUNT_SETTING)) {
            configValue = validatedSetting(configValue, WechatOfficialAccountSetting.class);
        }
        return configValue;
    }

    /**
     * 将平面配置转换为指定 DTO，统一兼容旧配置中的空串、字符串 null 和首尾空格。
     * 只用于提现及公众号设置，其他设置的数组、嵌套对象与富文本处理保持原有规则。
     *
     * @param configValue 配置 JSON
     * @param type 配置 DTO 类型，字段定义决定数字、布尔值和文本的最终类型
     * @param <T> 配置 DTO 类型
     * @return 完成类型转换的配置对象；读取历史配置时不执行保存约束
     */
    private <T> T parseSetting(String configValue, Class<T> type) {
        JSONObject json = JSONUtil.parseObj(configValue);
        json.replaceAll((key, value) -> value instanceof String ? ((String) value).trim() : value);
        json.entrySet().removeIf(entry -> entry.getValue() == null || "".equals(entry.getValue()) || "null".equals(entry.getValue()));
        try {
            return json.toBean(type);
        } catch (RuntimeException e) {
            throw new ServiceException("配置字段类型不正确");
        }
    }

    /**
     * 保存前统一转换类型并执行 DTO 的声明式校验，禁止绕过前端提交非法配置。
     *
     * @param configValue 配置 JSON
     * @param type 含校验约束的配置 DTO 类型
     * @param <T> 配置 DTO 类型
     * @return 按 DTO 字段类型序列化的配置 JSON
     * @throws ServiceException 配置类型或业务约束不合法
     */
    private <T> String validatedSetting(String configValue, Class<T> type) {
        T config = parseSetting(configValue, type);
        var violations = validator.validate(config);
        if (!violations.isEmpty()) throw new ServiceException(violations.iterator().next().getMessage());
        return JSONUtil.toJsonStr(config);
    }


    /**
     * 按配置类型返回对应 DTO，未配置时提供原有默认值；历史配置读取不执行保存约束。
     * @param settingEnum 配置类型，对应 li_setting.id
     * @return 对应配置 DTO
     * @throws ServiceException 不支持的配置类型
     */
    @Override
    public Object getConfig(SettingEnum settingEnum) {
        cache.remove(settingEnum.name());
        Setting setting = settingService.get(settingEnum.name());
        switch (settingEnum) {
            case BASE_SETTING:
                return setting == null ?
                        new BaseSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), BaseSetting.class);
            case WITHDRAWAL_SETTING:
                return setting == null ?
                        new WithdrawalSetting() :
                        parseSetting(setting.getSettingValue(), WithdrawalSetting.class);
            case DISTRIBUTION_SETTING:
                return distributionSettingSupport.getSetting();
            case EMAIL_SETTING:
                return setting == null ?
                        new EmailSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), EmailSetting.class);
            case GOODS_SETTING:
                return setting == null ?
                        new GoodsSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), GoodsSetting.class);
            case LOGISTICS_SETTING:
                return setting == null ?
                        new LogisticsSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), LogisticsSetting.class);
            case ORDER_SETTING:
                return setting == null ?
                        new OrderSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), OrderSetting.class);
            case OSS_SETTING:
                return setting == null ?
                        new OssSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), OssSetting.class);
            case SMS_SETTING:
                return setting == null ?
                        new SmsSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), SmsSetting.class);
            case POINT_SETTING:
                return setting == null ?
                        new PointSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), PointSetting.class);
            case QQ_CONNECT:
                return setting == null ?
                        new QQConnectSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), QQConnectSetting.class);
            case CONNECT_SETTING:
                return setting == null ?
                        new ConnectSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), ConnectSetting.class);
            case PAYMENT_SUPPORT:
                return setting == null ?
                        new PaymentSupportSetting(new PaymentSupportForm()) :
                        JSONUtil.toBean(setting.getSettingValue(), PaymentSupportSetting.class);
            case ALIPAY_PAYMENT:
                return setting == null ?
                        new AlipayPaymentSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), AlipayPaymentSetting.class);
            case WECHAT_CONNECT:
                return setting == null ?
                        new WechatConnectSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), WechatConnectSetting.class);
            case WECHAT_OFFICIAL_ACCOUNT_SETTING:
                return setting == null ?
                        new WechatOfficialAccountSetting() :
                        parseSetting(setting.getSettingValue(), WechatOfficialAccountSetting.class);
            case WECHAT_PAYMENT:
                return setting == null ?
                        new WechatPaymentSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), WechatPaymentSetting.class);
            case SECKILL_SETTING:
                return setting == null ?
                        new SeckillSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), SeckillSetting.class);
            case EXPERIENCE_SETTING:
                return setting == null ?
                        new ExperienceSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), ExperienceSetting.class);
            case IM_SETTING:
                return setting == null ?
                        new ImSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), ImSetting.class);
            case HOT_WORDS:
                return setting == null ?
                        new HotWordsSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), HotWordsSetting.class);
            case WX_CHANNELS:
                return setting == null ?
                        new WxChannelsSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), WxChannelsSetting.class);
            case LIVE_SETTING:
                return setting == null ?
                        new LiveSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), LiveSetting.class);
            case VERIFICATION_SETTING:
                return setting == null ?
                        new VerificationSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), VerificationSetting.class);
            case SECURITY_SETTING:
                return setting == null ?
                        new SecuritySetting() :
                        JSONUtil.toBean(setting.getSettingValue(), SecuritySetting.class);
            case THEME_SETTING:
                return setting == null ?
                        new ThemeSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), ThemeSetting.class);
            case CATEGORY_PAGE_SETTING:
                return setting == null ?
                        new CategoryPageSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), CategoryPageSetting.class);
            case  SENSITIVE_WORDS_SETTING:
                return setting == null ?
                        new SensitiveWordsSetting() :
                        JSONUtil.toBean(setting.getSettingValue(), SensitiveWordsSetting.class);
            default:
                throw new ServiceException(ResultCode.SETTING_NOT_TO_SET);
        }
    }
}
