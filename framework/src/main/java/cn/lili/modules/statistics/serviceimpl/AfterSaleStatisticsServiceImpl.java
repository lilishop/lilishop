package cn.lili.modules.statistics.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum;
import cn.lili.modules.statistics.mapper.AfterSaleStatisticsMapper;
import cn.lili.modules.statistics.service.AfterSaleStatisticsService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.Objects;

/**
 * 售后统计业务层实现
 *
 * @author Bulbasaur
 * @since 2020/12/9 11:30
 */
@Service
public class AfterSaleStatisticsServiceImpl extends ServiceImpl<AfterSaleStatisticsMapper, AfterSale> implements AfterSaleStatisticsService {


    @Override
    public Integer applyNum(String serviceType) {
        AuthUser authUser = Objects.requireNonNull(UserContext.getCurrentUser());
        LambdaQueryWrapper<AfterSale> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(AfterSale::getServiceStatus, AfterSaleStatusEnum.APPLY.name());
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(serviceType), AfterSale::getServiceType, serviceType);
        queryWrapper.eq(CharSequenceUtil.equals(authUser.getRole().name(), UserEnums.STORE.name()),
                AfterSale::getStoreId, authUser.getStoreId());
        return this.count(queryWrapper);
    }
}
