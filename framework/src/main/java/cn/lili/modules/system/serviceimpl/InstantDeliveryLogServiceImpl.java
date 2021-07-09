package cn.lili.modules.system.serviceimpl;

import cn.lili.modules.system.entity.dos.InstantDeliveryLog;
import cn.lili.modules.system.mapper.InstantDeliveryLogMapper;
import cn.lili.modules.system.service.InstantDeliveryLogService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 即时配送业务层实现
 *
 * @author Chopper
 * @date 2020/11/17 8:02 下午
 */
@Service
public class InstantDeliveryLogServiceImpl extends ServiceImpl<InstantDeliveryLogMapper, InstantDeliveryLog> implements InstantDeliveryLogService {

}