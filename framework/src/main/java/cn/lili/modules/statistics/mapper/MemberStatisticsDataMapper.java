package cn.lili.modules.statistics.mapper;

import cn.lili.modules.statistics.model.dos.MemberStatisticsData;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 会员统计数据处理层
 *
 * @author Bulbasaur
 * @date 2020/11/17 7:34 下午
 */
public interface MemberStatisticsDataMapper extends BaseMapper<MemberStatisticsData> {

    /**
     * 获取会员统计数量
     *
     * @param queryWrapper 查询条件
     * @return 会员统计数量
     */
    @Select("SELECT  COUNT(0)  FROM li_member  ${ew.customSqlSegment}")
    Integer customSqlQuery(@Param(Constants.WRAPPER) Wrapper queryWrapper);

}