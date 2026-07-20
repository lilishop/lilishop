package cn.lili.modules.statistics.mapper;

import cn.lili.modules.member.entity.vo.MemberDistributionVO;
import cn.lili.modules.statistics.entity.dos.MemberStatisticsData;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 客户统计数据处理层
 *
 * @author Bulbasaur
 * @since 2020/11/17 7:34 下午
 */
public interface MemberStatisticsMapper extends BaseMapper<MemberStatisticsData> {

    /**
     * 获取客户统计数量
     *
     * @param queryWrapper 查询条件
     * @return 客户统计数量
     */
    @Select("SELECT  COUNT(0)  FROM li_member  ${ew.customSqlSegment}")
    long customSqlQuery(@Param(Constants.WRAPPER) Wrapper queryWrapper);


    /**
     * 获取客户分布列表
     * @return 客户分布列表
     */
    @Select("select client_enum,count(0) as num from li_member group by client_enum")
    List<MemberDistributionVO> distribution();

    @Select("SELECT DATE_FORMAT(create_time,'%Y-%m-%d') AS day, COUNT(0) AS num FROM li_member ${ew.customSqlSegment}")
    List<java.util.Map<String, Object>> groupNewMemberByDay(@Param(Constants.WRAPPER) Wrapper queryWrapper);
}