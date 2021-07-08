package cn.lili.modules.statistics.mapper;

import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.statistics.model.vo.GoodsStatisticsDataVO;
import cn.lili.modules.statistics.model.vo.StoreStatisticsDataVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 店铺统计数据处理层
 *
 * @author Bulbasaur
 * @date 2020/11/17 7:34 下午
 */
public interface StoreStatisticsDataMapper extends BaseMapper<StoreFlow> {

    /**
     * 店铺统计列表
     *
     * @param page         分页
     * @param queryWrapper 查询参数
     * @return 店铺统计列表
     */
    @Select("SELECT store_id AS storeId,store_name AS storeName,SUM(final_price) AS price,SUM(num) AS num FROM li_store_flow ${ew.customSqlSegment}")
    List<StoreStatisticsDataVO> getStoreStatisticsData(IPage<GoodsStatisticsDataVO> page, @Param(Constants.WRAPPER) Wrapper<GoodsStatisticsDataVO> queryWrapper);

}