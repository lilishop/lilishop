package cn.lili.modules.statistics.service;

import cn.lili.modules.promotion.entity.dos.Seckill;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 秒杀统计
 *
 * @author Chopper
 * @since 2020/11/18 9:45 上午
 */
public interface SeckillStatisticsService extends IService<Seckill> {


    /**
     * 获取当前可参与的活动数量
     *
     * @return 可参与活动数量
     */
    long getApplyNum();

}