package cn.lili.modules.system.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.Brand;
import cn.lili.modules.system.entity.dos.AppVersion;
import cn.lili.modules.system.mapper.AppVersionMapper;
import cn.lili.modules.system.service.AppVersionService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


/**
 * APP版本控制业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 8:02 下午
 */
@Service
public class AppVersionServiceImpl extends ServiceImpl<AppVersionMapper, AppVersion> implements AppVersionService {

    @Override
    public AppVersion getAppVersion(String appType) {
        return this.baseMapper.getLatestVersion(appType);
    }

    @Override
    public boolean checkAppVersion(AppVersion appVersion) {
        //检测版本是否存在
        if (null != this.getOne(new LambdaQueryWrapper<AppVersion>()
                .eq(AppVersion::getVersion, appVersion.getVersion())
                .ne(appVersion.getId() != null, AppVersion::getId, appVersion.getId()))) {
            throw new ServiceException(ResultCode.APP_VERSION_EXIST);
        }
        return true;
    }
}
