package cn.lili.modules.system.serviceimpl;

import cn.lili.modules.system.entity.dos.ServiceNotice;
import cn.lili.modules.system.mapper.ServiceNoticeMapper;
import cn.lili.modules.system.service.ServiceNoticeService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 服务订阅消息业务层实现
 * @author Chopper
 * @since 2020/11/17 8:02 下午
 */
@Service
public class ServiceNoticeServiceImpl extends ServiceImpl<ServiceNoticeMapper, ServiceNotice> implements ServiceNoticeService {

}