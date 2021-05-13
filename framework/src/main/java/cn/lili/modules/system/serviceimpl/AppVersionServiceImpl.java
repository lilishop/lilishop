package cn.lili.modules.system.serviceimpl;

import cn.lili.modules.system.entity.dos.AppVersionDO;
import cn.lili.modules.system.mapper.AppVersionMapper;
import cn.lili.modules.system.service.AppVersionService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
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
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class AppVersionServiceImpl extends ServiceImpl<AppVersionMapper, AppVersionDO> implements AppVersionService {

}
