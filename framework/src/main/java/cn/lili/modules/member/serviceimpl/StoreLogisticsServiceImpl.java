package cn.lili.modules.member.serviceimpl;

import cn.lili.modules.member.mapper.StoreLogisticsMapper;
import cn.lili.modules.member.service.StoreLogisticsService;
import cn.lili.modules.store.entity.dos.StoreLogistics;
import cn.lili.modules.system.entity.vo.StoreLogisticsVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 物流公司业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 8:02 下午
 */
@Service
public class StoreLogisticsServiceImpl extends ServiceImpl<StoreLogisticsMapper, StoreLogistics> implements StoreLogisticsService {

    @Override
    public List<StoreLogisticsVO> getStoreLogistics(String storeId) {
        return this.baseMapper.getStoreLogistics(storeId);
    }

    @Override
    public List<StoreLogisticsVO> getStoreSelectedLogistics(String storeId) {
        return this.baseMapper.getSelectedStoreLogistics(storeId);

    }

    @Override
    public List<String> getStoreSelectedLogisticsName(String storeId) {
        return this.baseMapper.getSelectedStoreLogisticsName(storeId);
    }

    @Override
    public StoreLogistics add(String logisticsId, String storeId) {
        //判断是否已经选择过，如果没有选择则进行添加
        LambdaQueryWrapper<StoreLogistics> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(StoreLogistics::getLogisticsId, logisticsId);
        lambdaQueryWrapper.eq(StoreLogistics::getStoreId, storeId);
        if (this.getOne(lambdaQueryWrapper) == null) {
            StoreLogistics storeLogistics = new StoreLogistics(storeId, logisticsId);
            this.save(storeLogistics);
            return storeLogistics;
        }
        return null;
    }


}