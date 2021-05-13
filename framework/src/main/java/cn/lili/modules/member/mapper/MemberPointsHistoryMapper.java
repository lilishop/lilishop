package cn.lili.modules.member.mapper;

import cn.lili.modules.member.entity.dos.MemberPointsHistory;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;

/**
 * 会员积分历史数据处理层
 *
 * @author Bulbasaur
 * @date 2020-02-25 14:10:16
 */
public interface MemberPointsHistoryMapper extends BaseMapper<MemberPointsHistory> {

    @Select("SELECT SUM( variable_point ) FROM li_member_points_history WHERE point_type = #{pointType}")
    Long getALLMemberPointsHistoryVO(Integer pointType);

    @Select("SELECT SUM( variable_point ) FROM li_member_points_history WHERE point_type = #{pointType} AND member_id=#{memberId}")
    Long getMemberPointsHistoryVO(Integer pointType, String memberId);


}