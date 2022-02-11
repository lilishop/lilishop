package cn.lili.modules.payment.serviceimpl;

import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.mapper.RefundLogMapper;
import cn.lili.modules.payment.service.RefundLogService;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 退款日志 业务实现
 *
 * @author Chopper
 * @since 2020-12-19 09:25
 */
@Service
public class RefundLogServiceImpl extends ServiceImpl<RefundLogMapper, RefundLog> implements RefundLogService {

    @Override
    public RefundLog queryByAfterSaleSn(String sn) {
        return this.getOne(new LambdaUpdateWrapper<RefundLog>().eq(RefundLog::getAfterSaleNo, sn));
    }
}