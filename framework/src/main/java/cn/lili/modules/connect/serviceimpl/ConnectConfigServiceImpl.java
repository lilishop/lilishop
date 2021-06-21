package cn.lili.modules.connect.serviceimpl;

import cn.lili.modules.connect.entity.ConnectConfig;
import cn.lili.modules.connect.entity.enums.ConnectConfigEnum;
import cn.lili.modules.connect.entity.vo.ConnectConfigForm;
import cn.lili.modules.connect.mapper.ConnectConfigMapper;
import cn.lili.modules.connect.service.ConnectConfigService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

/**
 * 联合登陆配置接口实现
 *
 * @author Chopper
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ConnectConfigServiceImpl extends ServiceImpl<ConnectConfigMapper, ConnectConfig> implements ConnectConfigService {

    @Override
    public List<ConnectConfigForm> listForms() {

        List<ConnectConfigForm> formList = new ArrayList<>();
        for (int i = 0; i < ConnectConfigEnum.values().length; i++) {
            ConnectConfigEnum enums = ConnectConfigEnum.values()[i];
            ConnectConfigForm form = new ConnectConfigForm();
            form.setKey(enums.name());
            form.setName(enums.getName());
            form.setForm(enums.getForm());
            formList.add(form);
        }

        return formList;
    }

    @Override
    public ConnectConfig getConfig(String configKey) {
        QueryWrapper queryWrapper = new QueryWrapper();
        queryWrapper.eq("config_key", configKey);
        ConnectConfig connectConfig = this.getOne(queryWrapper);
        if (connectConfig == null) {
            return new ConnectConfig(configKey);
        }
        return connectConfig;
    }

    @Override
    public ConnectConfig saveConfig(ConnectConfig connectConfig) {
        return null;
    }
}