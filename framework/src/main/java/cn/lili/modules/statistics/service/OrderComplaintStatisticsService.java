package cn.lili.modules.statistics.service;

import cn.lili.modules.order.order.entity.dos.OrderComplaint;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 交易投诉统计
 *
 * @author paulG
 * @since 2020/12/4
 **/
public interface OrderComplaintStatisticsService extends IService<OrderComplaint> {

    /**
     * 待处理投诉数量
     *
     * @return 待处理投诉数量
     */
    long waitComplainNum();
}
