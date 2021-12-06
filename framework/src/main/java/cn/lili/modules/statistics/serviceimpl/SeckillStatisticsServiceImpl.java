package cn.lili.modules.statistics.serviceimpl;

import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.statistics.mapper.SeckillStatisticsMapper;
import cn.lili.modules.statistics.service.SeckillStatisticsService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 秒杀活动统计
 *
 * @author Chopper
 * @since 2020/8/21
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class SeckillStatisticsServiceImpl extends ServiceImpl<SeckillStatisticsMapper, Seckill> implements SeckillStatisticsService {


    @Override
    public Integer getApplyNum() {
        LambdaQueryWrapper<Seckill> queryWrapper = Wrappers.lambdaQuery();
        //秒杀申请时间未超过当前时间
        queryWrapper.ge(Seckill::getApplyEndTime, cn.hutool.core.date.DateUtil.date());
        queryWrapper.eq(Seckill::getPromotionStatus, PromotionStatusEnum.NEW.name());
        return this.count(queryWrapper);
    }

}