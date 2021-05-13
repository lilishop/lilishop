package cn.lili.modules.member.mapper;


import cn.lili.modules.member.entity.dos.MemberSign;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 会员签到数据处理层
 *
 * @author pikachu
 * @date 2020-02-25 14:10:16
 */
public interface MemberSignMapper extends BaseMapper<MemberSign> {

    @Select("SELECT * FROM li_member_sign WHERE TO_DAYS( NOW( ) ) - TO_DAYS( create_time) = 1 and member_id = #{memberId}")
    List<MemberSign> getBeforeMemberSign(String memberId);


    @Select("select * from li_member_sign ${ew.customSqlSegment}")
    List<MemberSign> getTodayMemberSign(@Param(Constants.WRAPPER) Wrapper<MemberSign> queryWrapper);

    @Select("SELECT * FROM li_member_sign WHERE DATE_FORMAT(create_time,'%Y%m') = #{time} and member_id = #{memberId}")
    List<MemberSign> getMonthMemberSign(String memberId, String time);

}