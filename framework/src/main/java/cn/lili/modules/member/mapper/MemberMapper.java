package cn.lili.modules.member.mapper;


import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.vo.MemberPointsStatisticsVO;
import cn.lili.modules.member.entity.vo.MemberVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 客户数据处理层
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
public interface MemberMapper extends BaseMapper<Member> {

    /**
     * 获取所有的客户手机号
     * @return 客户手机号
     */
    @Select("SELECT m.mobile FROM li_member m")
    List<String> getAllMemberMobile();

    @Select("SELECT m.*, COALESCE(mw.member_wallet, 0) AS member_wallet, mg.grade_name AS gradeName FROM li_member m LEFT JOIN li_member_wallet mw ON m.id = mw.member_id LEFT JOIN li_member_grade mg ON m.grade_id = mg.id ${ew.customSqlSegment}")
    IPage<MemberVO> pageByMemberVO(IPage<MemberVO> page, @Param(Constants.WRAPPER) Wrapper<Member> queryWrapper);

    @Select("SELECT m.*, COALESCE(mw.member_wallet, 0) AS member_wallet, mg.grade_name AS gradeName FROM li_member m LEFT JOIN li_member_wallet mw ON m.id = mw.member_id LEFT JOIN li_member_grade mg ON m.grade_id = mg.id WHERE m.id = #{id}")
    MemberVO getMemberVOById(@Param("id") String id);

    @Select("SELECT SUM( total_point ) AS totalPoint,SUM( point ) AS unUsedPoint FROM li_member")
    MemberPointsStatisticsVO queryMemberPointsStatistics();

}
