package cn.lili.modules.member.mapper;

import cn.lili.modules.member.entity.dos.MemberExperienceLog;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 客户经验值流水数据层
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
public interface MemberExperienceLogMapper extends BaseMapper<MemberExperienceLog> {

    /**
     * 获取用户在某经验值规则下累计获得经验值
     *
     * @param memberId 客户ID
     * @param ruleKey  规则编码
     * @return 累计经验值
     */
    @Select("SELECT IFNULL(SUM(variable_experience),0) FROM li_member_experience_log WHERE member_id = #{memberId} AND rule_key = #{ruleKey} AND delete_flag = 0")
    Long sumByMemberAndRule(@Param("memberId") String memberId, @Param("ruleKey") String ruleKey);

    /**
     * 按客户+规则统计经验值日志数量
     */
    @Select("SELECT COUNT(1) FROM li_member_experience_log WHERE member_id = #{memberId} AND rule_key = #{ruleKey} AND delete_flag = 0")
    Long countByMemberAndRule(@Param("memberId") String memberId, @Param("ruleKey") String ruleKey);

    /**
     * 按客户+规则+业务ID统计经验值日志数量（幂等）
     */
    @Select("SELECT COUNT(1) FROM li_member_experience_log WHERE member_id = #{memberId} AND rule_key = #{ruleKey} AND biz_id = #{bizId} AND delete_flag = 0")
    Long countByMemberRuleAndBiz(@Param("memberId") String memberId, @Param("ruleKey") String ruleKey, @Param("bizId") String bizId);

    /**
     * 按客户+规则+业务ID获取经验值变动值
     */
    @Select("SELECT variable_experience FROM li_member_experience_log WHERE member_id = #{memberId} AND rule_key = #{ruleKey} AND biz_id = #{bizId} AND delete_flag = 0 ORDER BY create_time DESC LIMIT 1")
    Long getVariableByMemberRuleAndBiz(@Param("memberId") String memberId, @Param("ruleKey") String ruleKey, @Param("bizId") String bizId);

    /**
     * 分页查询客户经验值流水
     */
    @Select("SELECT el.*, m.mobile AS memberMobile " +
            "FROM li_member_experience_log el " +
            "LEFT JOIN li_member m ON el.member_id = m.id ${ew.customSqlSegment}")
    IPage<MemberExperienceLog> pageWithMember(IPage<MemberExperienceLog> page,
                                              @Param(Constants.WRAPPER) Wrapper<MemberExperienceLog> queryWrapper);
}
