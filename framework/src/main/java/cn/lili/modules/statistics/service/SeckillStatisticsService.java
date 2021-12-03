package cn.lili.modules.statistics.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.vos.SeckillSearchParams;
import cn.lili.modules.promotion.entity.vos.SeckillVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
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
    Integer getApplyNum();

}