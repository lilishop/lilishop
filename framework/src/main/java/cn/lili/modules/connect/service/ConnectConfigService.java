package cn.lili.modules.connect.service;

import cn.lili.modules.connect.entity.ConnectConfig;
import cn.lili.modules.connect.entity.vo.ConnectConfigForm;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 联合登陆配置接口
 *
 * @author Chopper
 */
public interface ConnectConfigService extends IService<ConnectConfig> {

    /**
     * 获取所有配置项目
     *
     * @return
     */
    List<ConnectConfigForm> listForms();

    /**
     * 获取配置详情
     *
     * @return
     */
    ConnectConfig getConfig(String key);

    /**
     * 获取配置详情
     *
     * @return
     */
    ConnectConfig saveConfig(ConnectConfig connectConfig);
}