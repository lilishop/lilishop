package cn.lili.modules.statistics.serviceimpl;

import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.statistics.mapper.BillStatisticsMapper;
import cn.lili.modules.statistics.service.BillStatisticsService;
import cn.lili.modules.store.entity.dos.Bill;
import cn.lili.modules.store.entity.enums.BillStatusEnum;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 结算单统计
 *
 * @author Chopper
 * @since 2020/11/17 4:28 下午
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class BillStatisticsServiceImpl extends ServiceImpl<BillStatisticsMapper, Bill> implements BillStatisticsService {


    @Override
    public Integer billNum(BillStatusEnum billStatusEnum) {
        LambdaUpdateWrapper<Bill> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(Bill::getBillStatus, billStatusEnum.name());
        lambdaUpdateWrapper.eq(StringUtils.equals(UserContext.getCurrentUser().getRole().name(), UserEnums.STORE.name()),
                Bill::getStoreId, UserContext.getCurrentUser().getStoreId());
        return this.count(lambdaUpdateWrapper);
    }


}
