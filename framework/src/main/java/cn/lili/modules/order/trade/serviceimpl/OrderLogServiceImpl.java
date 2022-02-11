package cn.lili.modules.order.trade.serviceimpl;

import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.mapper.OrderLogMapper;
import cn.lili.modules.order.trade.service.OrderLogService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 订单日志业务层实现
 *
 * @author Chopper
 * @since 2020-02-25 14:10:16
 */
@Service
public class OrderLogServiceImpl extends ServiceImpl<OrderLogMapper, OrderLog> implements OrderLogService {

    @Override
    public List<OrderLog> getOrderLog(String orderSn) {
        LambdaQueryWrapper<OrderLog> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(OrderLog::getOrderSn, orderSn);
        return this.list(lambdaQueryWrapper);
    }
}