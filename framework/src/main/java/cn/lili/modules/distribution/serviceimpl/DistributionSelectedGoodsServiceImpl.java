package cn.lili.modules.distribution.serviceimpl;

import cn.lili.modules.distribution.entity.dos.DistributionSelectedGoods;
import cn.lili.modules.distribution.mapper.DistributionSelectedGoodsMapper;
import cn.lili.modules.distribution.service.DistributionSelectedGoodsService;
import cn.lili.modules.distribution.service.DistributionService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 分销选择商品接口实现
 *
 * @author pikachu
 * @since 2020-03-24 23:04:56
 */
@Service
public class DistributionSelectedGoodsServiceImpl extends ServiceImpl<DistributionSelectedGoodsMapper, DistributionSelectedGoods> implements DistributionSelectedGoodsService {

    /**
     * 分销员
     */
    @Autowired
    private DistributionService distributionService;

    @Override
    public boolean add(String distributionGoodsId) {
        //检查分销功能开关
        distributionService.checkDistributionSetting();

        String distributionId = distributionService.getDistribution().getId();
        DistributionSelectedGoods distributionSelectedGoods = new DistributionSelectedGoods(distributionId, distributionGoodsId);
        return this.save(distributionSelectedGoods);
    }

    @Override
    public boolean delete(String distributionGoodsId) {
        //检查分销功能开关
        distributionService.checkDistributionSetting();

        String distributionId = distributionService.getDistribution().getId();
        return this.remove(new LambdaQueryWrapper<DistributionSelectedGoods>()
                .eq(DistributionSelectedGoods::getDistributionGoodsId, distributionGoodsId)
                .eq(DistributionSelectedGoods::getDistributionId, distributionId));
    }

    @Override
    public boolean deleteByDistributionGoodsId(String distributionGoodsId) {
        return this.remove(new LambdaQueryWrapper<DistributionSelectedGoods>()
                .eq(DistributionSelectedGoods::getDistributionGoodsId, distributionGoodsId));
    }
}
