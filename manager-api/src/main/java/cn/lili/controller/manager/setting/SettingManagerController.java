package cn.lili.controller.manager.setting;

import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.support.DistributionSettingSupport;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.*;
import cn.lili.modules.system.entity.dto.connect.ConnectSetting;
import cn.lili.modules.system.entity.dto.connect.QQConnectSetting;
import cn.lili.modules.system.entity.dto.connect.WechatConnectSetting;
import cn.lili.modules.system.entity.dto.payment.AlipayPaymentSetting;
import cn.lili.modules.system.entity.dto.payment.PaymentSupportSetting;
import cn.lili.modules.system.entity.dto.payment.UnionPaymentSetting;
import cn.lili.modules.system.entity.dto.payment.WechatPaymentSetting;
import cn.lili.modules.system.entity.dto.payment.dto.PaymentSupportForm;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wxchannels.entity.dto.WxChannelsSetting;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;

/**
 * 管理端,系统设置接口
 *
 * @author Chopper
 * @since 2020/11/26 15:53
 */
@RestController
@Tag(name = "管理端,系统设置接口")
@RequestMapping("/manager/setting/setting")
public class SettingManagerController {
    @Autowired
    private SettingService settingService;
    /**
     * 缓存
     */
    @Autowired
    private Cache<String> cache;

    @Autowired
    private DistributionSettingSupport distributionSettingSupport;


    @DemoSite
    @Operation(summary = "更新配置")
    @Parameter(name = "key", description = "配置key", required = true)
    @Parameter(name = "configValue", description = "配置值", required = true)
    @PutMapping( "/put/{key}")
    public ResultMessage saveConfig(@PathVariable String key, @RequestBody String configValue) {
        SettingEnum settingEnum = SettingEnum.valueOf(key);
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
        return ResultUtil.success();
    }


    @DemoSite
    @Operation(summary = "查看配置")
    @Parameter(name = "key", description = "配置key", required = true)
    @GetMapping( "/get/{key}")
    public ResultMessage settingGet(@PathVariable String key) {
        return createSetting(key);
    }


    /**
     * 对配置进行过滤
     *
     * @param settingEnum
     * @param configValue
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
            configValue = normalizeWithdrawalSetting(configValue);
        }
        return configValue;
    }

    /**
     * 提现配置按字段类型入库，避免页面把空值保存成字符串 "null"、把金额保存成字符串。
     */
    private String normalizeWithdrawalSetting(String configValue) {
        JSONObject json = JSONUtil.parseObj(configValue);
        setOrRemove(json, "minPrice", toDoubleOrNull(json.get("minPrice")));
        setOrRemove(json, "fee", toDoubleOrNull(json.get("fee")));
        setOrRemove(json, "wechatAppId", toTextOrNull(json.get("wechatAppId")));
        setOrRemove(json, "wechatAppIdSource", toTextOrNull(json.get("wechatAppIdSource")));
        setOrRemove(json, "apply", toBooleanOrNull(json.get("apply")));
        setOrRemove(json, "type", toTextOrNull(json.get("type")));
        setOrRemove(json, "withdrawRule", toTextOrNull(json.get("withdrawRule")));
        WithdrawalSetting withdrawalSetting = json.toBean(WithdrawalSetting.class);
        return JSONUtil.toJsonStr(withdrawalSetting);
    }

    private Double toDoubleOrNull(Object value) {
        if (isBlankToken(value)) {
            return null;
        }
        try {
            return Double.valueOf(String.valueOf(value).trim());
        } catch (NumberFormatException e) {
            throw new ServiceException("提现金额或手续费必须是数字");
        }
    }

    private Boolean toBooleanOrNull(Object value) {
        if (isBlankToken(value)) {
            return null;
        }
        return Boolean.parseBoolean(String.valueOf(value).trim());
    }

    private String toTextOrNull(Object value) {
        if (isBlankToken(value)) {
            return null;
        }
        return String.valueOf(value).trim();
    }

    private void setOrRemove(JSONObject json, String key, Object value) {
        if (value == null) {
            json.remove(key);
        } else {
            json.set(key, value);
        }
    }

    private boolean isBlankToken(Object value) {
        if (value == null) {
            return true;
        }
        String text = String.valueOf(value).trim();
        return text.isEmpty() || "null".equals(text);
    }

    /**
     * 获取表单
     * 这里主要包含一个配置对象为空，导致转换异常问题的处理，解决配置项增加减少，带来的系统异常，无法直接配置
     *
     * @param key
     * @return
     * @throws InstantiationException
     * @throws IllegalAccessException
     */
    private ResultMessage createSetting(String key) {
        SettingEnum settingEnum = SettingEnum.valueOf(key);
        cache.remove(key);
        Setting setting = settingService.get(key);
        switch (settingEnum) {
            case BASE_SETTING:
                return setting == null ?
                        ResultUtil.data(new BaseSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), BaseSetting.class));
            case WITHDRAWAL_SETTING:
                return setting == null ?
                        ResultUtil.data(new WithdrawalSetting()) :
                        ResultUtil.data(JSONUtil.toBean(normalizeWithdrawalSetting(setting.getSettingValue()), WithdrawalSetting.class));
            case DISTRIBUTION_SETTING:
                return ResultUtil.data(distributionSettingSupport.getSetting());
            case EMAIL_SETTING:
                return setting == null ?
                        ResultUtil.data(new EmailSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), EmailSetting.class));
            case GOODS_SETTING:
                return setting == null ?
                        ResultUtil.data(new GoodsSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), GoodsSetting.class));
            case LOGISTICS_SETTING:
                return setting == null ?
                        ResultUtil.data(new LogisticsSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), LogisticsSetting.class));
            case ORDER_SETTING:
                return setting == null ?
                        ResultUtil.data(new OrderSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), OrderSetting.class));
            case OSS_SETTING:
                return setting == null ?
                        ResultUtil.data(new OssSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), OssSetting.class));
            case SMS_SETTING:
                return setting == null ?
                        ResultUtil.data(new SmsSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), SmsSetting.class));
            case POINT_SETTING:
                return setting == null ?
                        ResultUtil.data(new PointSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), PointSetting.class));
            case QQ_CONNECT:
                return setting == null ?
                        ResultUtil.data(new QQConnectSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), QQConnectSetting.class));
            case CONNECT_SETTING:
                return setting == null ?
                        ResultUtil.data(new ConnectSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), ConnectSetting.class));
            case PAYMENT_SUPPORT:
                return setting == null ?
                        ResultUtil.data(new PaymentSupportSetting(new PaymentSupportForm())) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), PaymentSupportSetting.class));
            case ALIPAY_PAYMENT:
                return setting == null ?
                        ResultUtil.data(new AlipayPaymentSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), AlipayPaymentSetting.class));
            case UNIONPAY_PAYMENT:
                return setting == null ?
                        ResultUtil.data(new UnionPaymentSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), UnionPaymentSetting.class));
            case WECHAT_CONNECT:
                return setting == null ?
                        ResultUtil.data(new WechatConnectSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), WechatConnectSetting.class));
            case WECHAT_PAYMENT:
                return setting == null ?
                        ResultUtil.data(new WechatPaymentSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), WechatPaymentSetting.class));
            case SECKILL_SETTING:
                return setting == null ?
                        ResultUtil.data(new SeckillSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), SeckillSetting.class));
            case EXPERIENCE_SETTING:
                return setting == null ?
                        ResultUtil.data(new ExperienceSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), ExperienceSetting.class));
            case IM_SETTING:
                return setting == null ?
                        ResultUtil.data(new ImSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), ImSetting.class));
            case HOT_WORDS:
                return setting == null ?
                        ResultUtil.data(new HotWordsSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), HotWordsSetting.class));
            case WX_CHANNELS:
                return setting == null ?
                        ResultUtil.data(new WxChannelsSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), WxChannelsSetting.class));
            case LIVE_SETTING:
                return setting == null ?
                        ResultUtil.data(new LiveSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), LiveSetting.class));
            case VERIFICATION_SETTING:
                return setting == null ?
                        ResultUtil.data(new VerificationSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), VerificationSetting.class));
            case SECURITY_SETTING:
                return setting == null ?
                        ResultUtil.data(new SecuritySetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), SecuritySetting.class));
            case THEME_SETTING:
                return setting == null ?
                        ResultUtil.data(new ThemeSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), ThemeSetting.class));
            case CATEGORY_PAGE_SETTING:
                return setting == null ?
                        ResultUtil.data(new CategoryPageSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), CategoryPageSetting.class));
            default:
                throw new ServiceException(ResultCode.SETTING_NOT_TO_SET);
        }
    }
}
