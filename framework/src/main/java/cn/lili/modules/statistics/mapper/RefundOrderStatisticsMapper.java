package cn.lili.modules.statistics.mapper;

import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.statistics.entity.vo.RefundOrderStatisticsDataVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 退款统计数据处理层
 *
 * @author Bulbasaur
 * @since 2020/12/10 11:22
 */
public interface RefundOrderStatisticsMapper extends BaseMapper<StoreFlow> {

    /**
     * 退款统计
     *
     * @param page         分页
     * @param queryWrapper 查询条件
     * @return 退款统计分页
     */
    @Select("SELECT refund_sn,store_name,member_name,name,specs,final_price FROM li_store_flow ${ew.customSqlSegment}")
    IPage<RefundOrderStatisticsDataVO> getRefundStatisticsData(IPage<RefundOrderStatisticsDataVO> page, @Param(Constants.WRAPPER) Wrapper<RefundOrderStatisticsDataVO> queryWrapper);
}
