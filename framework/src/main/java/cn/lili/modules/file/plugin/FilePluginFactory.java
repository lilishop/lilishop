package cn.lili.modules.file.plugin;

import cn.hutool.json.JSONUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.file.entity.enums.OssEnum;
import cn.lili.modules.file.plugin.impl.AliFilePlugin;
import cn.lili.modules.file.plugin.impl.MinioFilePlugin;
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


    /**
     * 获取oss client
     *
     * @return
     */
    public FilePlugin filePlugin() {

        OssSetting ossSetting = null;
        try {
            Setting setting = settingService.get(SettingEnum.OSS_SETTING.name());

            ossSetting = JSONUtil.toBean(setting.getSettingValue(), OssSetting.class);


            switch (OssEnum.valueOf(ossSetting.getType())) {

                case MINIO:
                    return new MinioFilePlugin(ossSetting);
                case ALI_OSS:
                    return new AliFilePlugin(ossSetting);
                default:
                    throw new ServiceException();
            }
        } catch (Exception e) {
            throw new ServiceException();
        }
    }


}
