package cn.lili.modules.statistics.service;

import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 售后统计业务层
 *
 * @author Bulbasaur
 * @since 2020/12/9 11:06
 */
public interface AfterSaleStatisticsService extends IService<AfterSale> {

    /**
     * 获取待处理售后数量
     *
     * @param serviceType 售后类型
     * @return 待处理售后数量
     */
    Integer applyNum(String serviceType);
}