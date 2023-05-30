package cn.lili.modules.statistics.serviceimpl;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.lili.modules.statistics.mapper.StoreStatisticsMapper;
import cn.lili.modules.statistics.service.StoreStatisticsService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * 商品统计业务层实现
 *
 * @author Bulbasaur
 * @since 2020/12/9 11:30
 */
@Service
public class StoreStatisticsServiceImpl extends ServiceImpl<StoreStatisticsMapper, Store> implements StoreStatisticsService {


    @Override
    public long auditNum() {
        LambdaQueryWrapper<Store> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(Store::getStoreDisable, StoreStatusEnum.APPLYING.name());
        return this.count(queryWrapper);
    }

    @Override
    public long storeNum() {
        LambdaQueryWrapper<Store> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(Store::getStoreDisable, StoreStatusEnum.OPEN.name());
        return this.count(queryWrapper);
    }

    @Override
    public long todayStoreNum() {
        LambdaQueryWrapper<Store> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(Store::getStoreDisable, StoreStatusEnum.OPEN.name());
        queryWrapper.ge(Store::getCreateTime, DateUtil.beginOfDay(new DateTime()));
        return this.count(queryWrapper);
    }

}
