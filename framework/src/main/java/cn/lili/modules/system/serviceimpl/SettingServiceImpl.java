package cn.lili.modules.system.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.mapper.SettingMapper;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 配置业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 3:52 下午
 */
@Service
public class SettingServiceImpl extends ServiceImpl<SettingMapper, Setting> implements SettingService {

    @Autowired
    private Cache<String> cache;

    @Override
    public Setting get(String key) {
        return this.getById(key);
    }

    @Override
    public boolean saveUpdate(Setting setting) {
        boolean result = this.saveOrUpdate(setting);
        if (result) {
            //删除设置缓存，保证保存即时生效
            cache.remove(CachePrefix.SETTING.getPrefix(setting.getId()));
        }
        return result;
    }
}