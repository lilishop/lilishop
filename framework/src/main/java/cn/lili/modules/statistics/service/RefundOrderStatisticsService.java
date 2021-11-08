package cn.lili.modules.statistics.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.RefundOrderStatisticsDataVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 退款订单统计业务层
 *
 * @author Bulbasaur
 * @since 2020/12/9 11:06
 */
public interface RefundOrderStatisticsService extends IService<StoreFlow> {


    /**
     * 查询订单统计分页
     *
     * @param statisticsQueryParam 查询参数
     * @param pageVO               分页
     * @return 退款统计
     */
    IPage<RefundOrderStatisticsDataVO> getRefundOrderStatisticsData(PageVO pageVO, StatisticsQueryParam statisticsQueryParam);

    /**
     * 查询退款订单统计金额
     *
     * @param statisticsQueryParam 查询参数
     * @return 退款统计金额
     */
    Double getRefundOrderStatisticsPrice(StatisticsQueryParam statisticsQueryParam);
}
