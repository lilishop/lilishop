package cn.lili.modules.order.aftersale.serviceimpl;

import cn.lili.modules.order.aftersale.entity.dos.AfterSaleLog;
import cn.lili.modules.order.aftersale.mapper.AfterSaleLogMapper;
import cn.lili.modules.order.aftersale.service.AfterSaleLogService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 订单日志业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 7:37 下午
 */
@Service
public class AfterSaleLogServiceImpl extends ServiceImpl<AfterSaleLogMapper, AfterSaleLog> implements AfterSaleLogService {

    @Override
    public List<AfterSaleLog> getAfterSaleLog(String sn) {
        QueryWrapper queryWrapper = Wrappers.query();
        queryWrapper.eq("sn", sn);
        return this.list(queryWrapper);
    }
}