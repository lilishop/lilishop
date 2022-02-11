package cn.lili.modules.statistics.serviceimpl;

import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.modules.statistics.mapper.SeckillStatisticsMapper;
import cn.lili.modules.statistics.service.SeckillStatisticsService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * 秒杀活动统计
 *
 * @author Chopper
 * @since 2020/8/21
 */
@Service
public class SeckillStatisticsServiceImpl extends ServiceImpl<SeckillStatisticsMapper, Seckill> implements SeckillStatisticsService {


    @Override
    public long getApplyNum() {
        QueryWrapper<Seckill> queryWrapper = Wrappers.query();
        //秒杀申请时间未超过当前时间
        queryWrapper.ge("apply_end_time", cn.hutool.core.date.DateUtil.date());
        queryWrapper.and(PromotionTools.queryPromotionStatus(PromotionsStatusEnum.START));
        return this.count(queryWrapper);
    }

}