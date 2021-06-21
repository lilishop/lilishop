package cn.lili.modules.broadcast.serviceimpl;

import cn.lili.modules.broadcast.entity.dos.StudioCommodity;
import cn.lili.modules.broadcast.mapper.StudioCommodityMapper;
import cn.lili.modules.broadcast.service.StudioCommodityService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * 直播间-商品关联业务层实现
 *
 * @author Bulbasaur
 * @date: 2021/5/17 3:20 下午
 */
@Service
public class StudioCommodityServiceImpl extends ServiceImpl<StudioCommodityMapper, StudioCommodity> implements StudioCommodityService {
}
