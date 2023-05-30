package cn.lili.modules.sms.plugin;

import cn.hutool.json.JSONUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.sms.entity.enums.SmsEnum;
import cn.lili.modules.sms.plugin.impl.AliSmsPlugin;
import cn.lili.modules.sms.plugin.impl.HuaweiSmsPlugin;
import cn.lili.modules.sms.plugin.impl.TencentSmsPlugin;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.SmsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 短信服务抽象工厂 直接返回操作类
 *
 * @author Bulbasaur
 * @version v1.0
 * @since 2023-02-16
 */
@Component
public class SmsPluginFactory {

    @Autowired
    private SettingService settingService;


    /**
     * 获取oss client
     *
     * @return
     */
    public SmsPlugin smsPlugin() {

        SmsSetting smsSetting = null;
        try {
            Setting setting = settingService.get(SettingEnum.SMS_SETTING.name());

            smsSetting = JSONUtil.toBean(setting.getSettingValue(), SmsSetting.class);


            switch (SmsEnum.valueOf(smsSetting.getType())) {

                case ALI:
                    return new AliSmsPlugin(smsSetting);
                case TENCENT:
                    return new TencentSmsPlugin(smsSetting);
                case HUAWEI:
                    return new HuaweiSmsPlugin(smsSetting);
                default:
                    throw new ServiceException();
            }
        } catch (Exception e) {
            throw new ServiceException();
        }
    }
}
