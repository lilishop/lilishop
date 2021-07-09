package cn.lili.modules.statistics.mapper;

import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.statistics.model.vo.OrderStatisticsDataVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 订单统计数据处理层
 *
 * @author Bulbasaur
 * @date 2020/11/17 7:34 下午
 */
public interface OrderStatisticsDataMapper extends BaseMapper<StoreFlow> {

    /**
     * 获取订单统计数据
     * @param queryWrapper 查询条件
     * @return 订单统计列表
     */
    @Select("SELECT DATE_FORMAT(create_time,'%Y-%m-%d') AS create_time,sum(flow_price) AS price FROM li_order " +
            " ${ew.customSqlSegment}")
    List<OrderStatisticsDataVO> getOrderStatisticsData(@Param(Constants.WRAPPER) Wrapper queryWrapper);

    /**
     * 订单数量
     * @param queryWrapper 查询条件
     * @return 订单数量
     */
    @Select("SELECT count(0) FROM li_order ${ew.customSqlSegment}")
    Integer count(@Param(Constants.WRAPPER) Wrapper queryWrapper);

}