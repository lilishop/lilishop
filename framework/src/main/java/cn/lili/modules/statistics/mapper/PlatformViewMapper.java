package cn.lili.modules.statistics.mapper;

import cn.lili.modules.statistics.entity.dos.PlatformViewData;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 平台流量数据
 *
 * @author Bulbasaur
 * @since 2020/11/17 7:34 下午
 */
public interface PlatformViewMapper extends BaseMapper<PlatformViewData> {
    /**
     * UV流量统计
     *
     * @param queryWrapper 查询条件
     * @return UV流量统计数量
     */
    @Select("SELECT sum(uv_num) FROM li_s_platform_view_data ${ew.customSqlSegment}")
    Integer count(@Param(Constants.WRAPPER) QueryWrapper queryWrapper);
}