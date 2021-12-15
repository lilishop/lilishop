package cn.lili.modules.statistics.service;

import cn.lili.modules.store.entity.dos.Store;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 店铺统计业务层
 *
 * @author Bulbasaur
 * @since 2020/12/9 11:06
 */
public interface StoreStatisticsService extends IService<Store> {

    /**
     * 获取待审核店铺数量
     *
     * @return 待审核店铺数量
     */
    long auditNum();

    /**
     * 获取所有店铺数量
     *
     * @return 店铺总数
     */
    long storeNum();

    /**
     * 获取今天的店铺数量
     *
     * @return 今天的店铺数量
     */
    long todayStoreNum();
}