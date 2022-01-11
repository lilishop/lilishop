package cn.lili.modules.statistics.serviceimpl;

import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.entity.enums.FlowTypeEnum;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.enums.TimeTypeEnum;
import cn.lili.modules.statistics.entity.vo.RefundOrderStatisticsDataVO;
import cn.lili.modules.statistics.mapper.RefundOrderStatisticsMapper;
import cn.lili.modules.statistics.service.RefundOrderStatisticsService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

/**
 * 退款订单统计业务层实现
 *
 * @author Bulbasaur
 * @since 2020/12/10 11:30
 */
@Service
public class RefundOrderStatisticsServiceImpl extends ServiceImpl<RefundOrderStatisticsMapper, StoreFlow> implements RefundOrderStatisticsService {

    @Override
    public IPage<RefundOrderStatisticsDataVO> getRefundOrderStatisticsData(PageVO pageVO, StatisticsQueryParam statisticsQueryParam) {
        QueryWrapper queryWrapper = getQueryWrapper(statisticsQueryParam);
        return this.baseMapper.getRefundStatisticsData(PageUtil.initPage(pageVO), queryWrapper);
    }

    @Override
    public Double getRefundOrderStatisticsPrice(StatisticsQueryParam statisticsQueryParam) {

        QueryWrapper queryWrapper = getQueryWrapper(statisticsQueryParam);
        queryWrapper.select("SUM(final_price) AS price");
        return (Double) this.getMap(queryWrapper).get("price");
    }


    private QueryWrapper getQueryWrapper(StatisticsQueryParam statisticsQueryParam) {

        QueryWrapper queryWrapper = Wrappers.query();

        //判断搜索类型是：年、月
        if (statisticsQueryParam.getTimeType().equals(TimeTypeEnum.MONTH.name())) {
            queryWrapper.between("create_time", DateUtil.getBeginTime(statisticsQueryParam.getYear(), statisticsQueryParam.getMonth()), DateUtil.getEndTime(statisticsQueryParam.getYear(), statisticsQueryParam.getMonth()));
        } else {
            queryWrapper.between("create_time", DateUtil.getBeginTime(statisticsQueryParam.getYear(), 1), DateUtil.getEndTime(statisticsQueryParam.getYear(), 12));
        }

        //设置店铺ID
        queryWrapper.eq(!StringUtils.isEmpty(statisticsQueryParam.getStoreId()), "store_id", statisticsQueryParam.getStoreId());

        //设置为退款查询
        queryWrapper.eq("flow_type", FlowTypeEnum.REFUND);
        return queryWrapper;
    }
}
