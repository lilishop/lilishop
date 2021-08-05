package cn.lili.modules.member.mapper;

import cn.lili.modules.member.entity.dos.MemberPointsHistory;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;

/**
 * 会员积分历史数据处理层
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
public interface MemberPointsHistoryMapper extends BaseMapper<MemberPointsHistory> {

    /**
     * 获取所有用户的积分历史VO
     *
     * @param pointType 积分类型
     * @return
     */
    @Select("SELECT SUM( variable_point ) FROM li_member_points_history WHERE point_type = #{pointType}")
    Long getALLMemberPointsHistoryVO(String pointType);

    /**
     * 获取用户的积分数量
     *
     * @param pointType 积分类型
     * @param memberId  会员ID
     * @return 积分数量
     */
    @Select("SELECT SUM( variable_point ) FROM li_member_points_history WHERE point_type = #{pointType} AND member_id=#{memberId}")
    Long getMemberPointsHistoryVO(String pointType, String memberId);


}