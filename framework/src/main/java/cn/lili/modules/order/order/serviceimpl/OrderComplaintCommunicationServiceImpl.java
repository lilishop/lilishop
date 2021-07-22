package cn.lili.modules.order.order.serviceimpl;

import cn.lili.mybatis.util.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.order.entity.dos.OrderComplaintCommunication;
import cn.lili.modules.order.order.entity.vo.OrderComplaintCommunicationSearchParams;
import cn.lili.modules.order.order.entity.vo.OrderComplaintCommunicationVO;
import cn.lili.modules.order.order.mapper.OrderComplainCommunicationMapper;
import cn.lili.modules.order.order.service.OrderComplaintCommunicationService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * 交易投诉通信业务层实现
 *
 * @author paulG
 * @since 2020/12/5
 **/
@Service
public class OrderComplaintCommunicationServiceImpl extends ServiceImpl<OrderComplainCommunicationMapper, OrderComplaintCommunication> implements OrderComplaintCommunicationService {

    @Override
    public boolean addCommunication(OrderComplaintCommunicationVO communicationVO) {
        return this.save(communicationVO);
    }

    @Override
    public IPage<OrderComplaintCommunication> getCommunication(OrderComplaintCommunicationSearchParams searchParams, PageVO pageVO) {
        return this.page(PageUtil.initPage(pageVO), searchParams.lambdaQueryWrapper());
    }
}
