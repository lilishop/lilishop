package cn.lili.modules.system.serviceimpl;

import cn.lili.modules.system.entity.dos.AppVersion;
import cn.lili.modules.system.mapper.AppVersionMapper;
import cn.lili.modules.system.service.AppVersionService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


/**
 * APP版本控制业务层实现
 *
 * @author Chopper
 * @date 2020/11/17 8:02 下午
 */
@Service
@Transactional
public class AppVersionServiceImpl extends ServiceImpl<AppVersionMapper, AppVersion> implements AppVersionService {

    @Override
    public AppVersion getAppVersion(String appType) {
        return this.baseMapper.getLatestVersion(appType);
    }
}
