package cn.lili.modules.payment.serviceimpl;

import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.mapper.RefundLogMapper;
import cn.lili.modules.payment.service.RefundLogService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 退款日志 业务实现
 *
 * @author Chopper
 * @date 2020-12-19 09:25
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class RefundLogServiceImpl extends ServiceImpl<RefundLogMapper, RefundLog> implements RefundLogService {


}