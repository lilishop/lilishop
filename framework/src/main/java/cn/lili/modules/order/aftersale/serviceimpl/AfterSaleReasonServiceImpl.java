package cn.lili.modules.order.aftersale.serviceimpl;

import cn.lili.modules.order.aftersale.entity.dos.AfterSaleReason;
import cn.lili.modules.order.aftersale.mapper.AfterSaleReasonMapper;
import cn.lili.modules.order.aftersale.service.AfterSaleReasonService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 售后原因业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 7:38 下午
 */
@Service
public class AfterSaleReasonServiceImpl extends ServiceImpl<AfterSaleReasonMapper, AfterSaleReason> implements AfterSaleReasonService {


    @Override
    public List<AfterSaleReason> afterSaleReasonList(String serviceType) {
        LambdaQueryWrapper<AfterSaleReason> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(AfterSaleReason::getServiceType, serviceType);
        return this.list(lambdaQueryWrapper);
    }

    @Override
    public AfterSaleReason editAfterSaleReason(AfterSaleReason afterSaleReason) {
        LambdaUpdateWrapper<AfterSaleReason> lambdaQueryWrapper = Wrappers.lambdaUpdate();
        lambdaQueryWrapper.eq(AfterSaleReason::getId, afterSaleReason.getId());
        lambdaQueryWrapper.set(AfterSaleReason::getReason, afterSaleReason.getReason());
        lambdaQueryWrapper.set(AfterSaleReason::getServiceType, afterSaleReason.getServiceType());
        this.update(lambdaQueryWrapper);
        return afterSaleReason;
    }
}