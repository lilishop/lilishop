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
     * 分页查询客户信息，关联钱包余额和等级名称，缺少钱包时余额按 0 返回。
     *
     * @param page 分页参数
     * @param queryWrapper 服务端构建的查询条件，关联字段需使用表别名，禁止拼接用户 SQL
     * @return 客户展示信息分页
     */
    @Select("SELECT m.*, COALESCE(mw.member_wallet, 0) AS member_wallet, mg.grade_name AS gradeName FROM li_member m LEFT JOIN li_member_wallet mw ON m.id = mw.member_id LEFT JOIN li_member_grade mg ON m.grade_id = mg.id ${ew.customSqlSegment}")
    IPage<MemberVO> pageByMemberVO(IPage<MemberVO> page, @Param(Constants.WRAPPER) Wrapper<Member> queryWrapper);

    /**
     * 查询指定客户及其钱包余额、等级名称。
     *
     * @param id li_member 主键
     * @return 客户信息，不存在时返回 null
     */
    @Select("SELECT m.*, COALESCE(mw.member_wallet, 0) AS member_wallet, mg.grade_name AS gradeName FROM li_member m LEFT JOIN li_member_wallet mw ON m.id = mw.member_id LEFT JOIN li_member_grade mg ON m.grade_id = mg.id WHERE m.id = #{id}")
    MemberVO getMemberVOById(@Param("id") String id);

    /**
     * 汇总所有客户累计积分 total_point 和剩余积分 point，不额外过滤客户状态。
     *
     * @return 积分统计，空表的 SUM 结果可能为 null
     */
    @Select("SELECT SUM( total_point ) AS totalPoint,SUM( point ) AS unUsedPoint FROM li_member")
    MemberPointsStatisticsVO queryMemberPointsStatistics();

}
