package cn.lili.modules.file.plugin;

import cn.hutool.core.text.CharSequenceUtil;
import com.alibaba.fastjson2.JSON;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.LocalFileProperties;
import cn.lili.modules.file.entity.enums.OssEnum;
import cn.lili.modules.file.plugin.impl.AliFilePlugin;
import cn.lili.modules.file.plugin.impl.HuaweiFilePlugin;
import cn.lili.modules.file.plugin.impl.LocalFilePlugin;
import cn.lili.modules.file.plugin.impl.MinioFilePlugin;
import cn.lili.modules.file.plugin.impl.TencentFilePlugin;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.OssSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 文件服务抽象工厂 直接返回操作类
 *
 * @author Chopper
 * @version v1.0
 * 2022-06-06 11:35
 */
@Component
public class FilePluginFactory {


    @Autowired
    private SettingService settingService;
    @Autowired
    private LocalFileProperties localFileProperties;


    /**
     * 获取oss client
     *
     * @return
     */
    public FilePlugin filePlugin() {

        OssSetting ossSetting = null;
        try {
            Setting setting = settingService.get(SettingEnum.OSS_SETTING.name());
            if (setting == null || CharSequenceUtil.isBlank(setting.getSettingValue())) {
                ossSetting = this.createDefaultLocalSetting();
            } else {
                ossSetting = JSON.parseObject(setting.getSettingValue(), OssSetting.class);
            }

            switch (OssEnum.valueOf(ossSetting.getType())) {

                case MINIO:
                    return new MinioFilePlugin(ossSetting);
                case ALI_OSS:
                    return new AliFilePlugin(ossSetting);
                case HUAWEI_OBS:
                    return new HuaweiFilePlugin(ossSetting);
                case TENCENT_COS:
                    return new TencentFilePlugin(ossSetting);
                case LOCAL:
                    fillLocalDefault(ossSetting);
                    return new LocalFilePlugin(ossSetting);
                default:
                    throw new ServiceException();
            }
        } catch (Exception e) {
            throw new ServiceException();
        }
    }

    private OssSetting createDefaultLocalSetting() {
        OssSetting ossSetting = new OssSetting();
        ossSetting.setType(OssEnum.LOCAL.name());
        fillLocalDefault(ossSetting);
        return ossSetting;
    }

    private void fillLocalDefault(OssSetting ossSetting) {
        if (CharSequenceUtil.isBlank(ossSetting.getLocalFilePath())) {
            ossSetting.setLocalFilePath(localFileProperties.getPath());
        }
        if (CharSequenceUtil.isBlank(ossSetting.getLocalFileUrlPrefix())) {
            ossSetting.setLocalFileUrlPrefix(localFileProperties.getUrlPrefix());
        }
    }

}
