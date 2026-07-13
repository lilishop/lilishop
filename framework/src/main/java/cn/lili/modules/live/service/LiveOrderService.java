package cn.lili.modules.live.service;

import cn.lili.modules.live.entity.vos.LiveOrderStatisticsVO;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import com.baomidou.mybatisplus.core.metadata.IPage;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
public interface LiveOrderService {

    /**
     * 查询订单列表
     * @param orderSearchParams
     * @return
     */
    IPage<OrderSimpleVO> queryOrderPage(OrderSearchParams orderSearchParams);

    /**
     * 数据统计
     * @param liveRoomId
     * @return
     */
    LiveOrderStatisticsVO statistics(String liveRoomId);

}
