package cn.lili.modules.message.serviceimpl;

import cn.lili.modules.message.entity.dos.ShortLink;
import cn.lili.modules.message.mapper.ShortLinkMapper;
import cn.lili.modules.message.service.ShortLinkService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 短链接 业务实现
 * @author Chopper
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class ShortLinkServiceImpl extends ServiceImpl<ShortLinkMapper, ShortLink> implements ShortLinkService {

}