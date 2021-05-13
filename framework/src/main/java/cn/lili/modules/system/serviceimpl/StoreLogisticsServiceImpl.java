package cn.lili.modules.system.serviceimpl;

import cn.lili.common.security.context.UserContext;
import cn.lili.modules.store.entity.dos.StoreLogistics;
import cn.lili.modules.system.entity.vo.StoreLogisticsVO;
import cn.lili.modules.system.mapper.StoreLogisticsMapper;
import cn.lili.modules.system.service.StoreLogisticsService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 物流公司业务层实现
 *
 * @author Chopper
 * @date 2020/11/17 8:02 下午
 */
@Service
@Transactional
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class StoreLogisticsServiceImpl extends ServiceImpl<StoreLogisticsMapper, StoreLogistics> implements StoreLogisticsService {

    private final StoreLogisticsMapper storeLogisticsMapper;

    @Override
    public List<StoreLogisticsVO> getStoreLogistics() {
        return storeLogisticsMapper.getStoreLogistics(UserContext.getCurrentUser().getStoreId());
    }

    @Override
    public List<StoreLogisticsVO> getStoreSelectedLogistics() {
        return storeLogisticsMapper.getSelectedStoreLogistics(UserContext.getCurrentUser().getStoreId());

    }

    @Override
    public StoreLogistics add(String logisticsId) {
        //判断是否已经选择过，如果没有选择则进行添加
        LambdaQueryWrapper<StoreLogistics> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(StoreLogistics::getLogisticsId, logisticsId);
        lambdaQueryWrapper.eq(StoreLogistics::getStoreId, UserContext.getCurrentUser().getStoreId());
        if (this.getOne(lambdaQueryWrapper) == null) {
            StoreLogistics storeLogistics = new StoreLogistics(UserContext.getCurrentUser().getStoreId(), logisticsId);
            this.save(storeLogistics);
            return storeLogistics;
        }
        return null;
    }


}