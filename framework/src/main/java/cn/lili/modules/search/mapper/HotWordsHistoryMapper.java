package cn.lili.modules.search.mapper;

import cn.lili.modules.search.entity.dos.CustomWords;
import cn.lili.modules.search.entity.dos.HotWordsHistory;
import cn.lili.modules.search.entity.vo.HotWordsHistoryVO;
import cn.lili.modules.statistics.entity.vo.OrderStatisticsDataVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 搜索热词历史记录数据处理层
 *
 * @author paulG
 * @since 2020/10/15
 **/
public interface HotWordsHistoryMapper extends BaseMapper<HotWordsHistory> {

    /**
     * 获取订单统计数据
     *
     * @param queryWrapper 查询条件
     * @return 订单统计列表
     */
    @Select("SELECT sum(score) as score,keywords FROM li_hot_words_history " +" ${ew.customSqlSegment}")
    List<HotWordsHistory> statistics(@Param(Constants.WRAPPER) Wrapper queryWrapper);

}
