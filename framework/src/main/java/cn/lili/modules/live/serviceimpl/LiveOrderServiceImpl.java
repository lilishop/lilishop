package cn.lili.modules.live.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.modules.live.entity.vos.LiveOrderStatisticsVO;
import cn.lili.modules.live.service.LiveOperateService;
import cn.lili.modules.live.service.LiveOrderService;
import cn.lili.modules.live.service.LiveRoomService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.service.OrderService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Service
@RequiredArgsConstructor
public class LiveOrderServiceImpl implements LiveOrderService {

    private final OrderService orderService;

    private final Cache cache;

    private final LiveRoomService liveRoomService;

    private final LiveOperateService liveOperateService;

    @Override
    public IPage<OrderSimpleVO> queryOrderPage(OrderSearchParams orderSearchParams) {
        return orderService.queryManageByParams(orderSearchParams);
    }

    @Override
    public LiveOrderStatisticsVO statistics(String liveRoomId) {
        LiveOrderStatisticsVO orderStatisticsVO = new LiveOrderStatisticsVO();

        OrderSearchParams orderSearchParams = new OrderSearchParams();
        orderSearchParams.setLiveRoomId(liveRoomId);
        List<String> statusList = Arrays.asList(
                OrderStatusEnum.PAID.name(),
                OrderStatusEnum.COMPLETED.name(),
                OrderStatusEnum.UNDELIVERED.name(),
                OrderStatusEnum.DELIVERED.name(),
                OrderStatusEnum.STAY_PICKED_UP.name(),
                // 注意这里你原来漏了.name()
                OrderStatusEnum.PARTS_DELIVERED.name(),
                OrderStatusEnum.TAKE.name()
        );
        orderSearchParams.setOrderStatusList(statusList);

        List<Order> orderList = orderService.queryListByParams(orderSearchParams);

        if (orderList != null && !orderList.isEmpty()) {

            long salesVolume = orderList.stream()
                    // 提取 buyerId
                    .map(Order::getMemberId)
                    // 去重
                    .distinct()
                    // 统计数量
                    .count();

            double totalAmount = orderList.stream()
                    // 转换为 DoubleStream，避免拆装箱
                    .mapToDouble(Order::getFlowPrice)
                    // 求和
                    .sum();

            orderStatisticsVO.setTotalAmount(totalAmount);
            orderStatisticsVO.setSalesVolume(orderList.size());
            orderStatisticsVO.setDealUserCount(salesVolume);

            Long viewCount = liveOperateService.getCacheView(liveRoomId);
            if (viewCount > 0 && salesVolume > 0) {
                orderStatisticsVO.setConversionRate(((double) salesVolume / viewCount) * 100);
            } else {
                orderStatisticsVO.setConversionRate(0D);
            }

            if (totalAmount > 0) {
                // 2. 注意这里的强制类型转换 (double)，否则两个整数相除会丢失小数部分！
                double rawValue = ((double) totalAmount / orderList.size()) * 100;

                // 3. 经典四舍五入保留两位小数算法：乘 100 -> 四舍五入取整 -> 除 100.0
                double roundedValue = Math.round(rawValue * 100) / 100.0;
                orderStatisticsVO.setAverageOrderValue(roundedValue);
            } else {
                orderStatisticsVO.setAverageOrderValue(0D);
            }
        }


        return orderStatisticsVO;
    }


}
