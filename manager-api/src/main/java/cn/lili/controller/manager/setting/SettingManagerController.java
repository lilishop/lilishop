package cn.lili.controller.manager.setting;

import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
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
        return configValue;
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
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), WithdrawalSetting.class));
            case DISTRIBUTION_SETTING:
                return setting == null ?
                        ResultUtil.data(new DistributionSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), DistributionSetting.class));
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
            case THEME_SETTING:
                return setting == null ?
                        ResultUtil.data(new ThemeSetting()) :
                        ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), ThemeSetting.class));
            default:
                throw new ServiceException(ResultCode.SETTING_NOT_TO_SET);
        }
    }
}
