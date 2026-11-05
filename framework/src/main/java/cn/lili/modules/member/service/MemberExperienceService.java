package cn.lili.modules.member.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.MemberExperienceLog;
import cn.lili.modules.member.entity.enums.ExperienceRuleEnum;
import cn.lili.modules.member.entity.vo.ExperienceRuleProgressListVO;
import cn.lili.modules.member.entity.vo.ExperienceRuleProgressVO;
import com.baomidou.mybatisplus.core.metadata.IPage;

import java.util.List;

/**
 * 客户经验值服务
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
public interface MemberExperienceService {

    /**
     * 发放经验值（固定值场景）
     *
     * @param memberId 客户ID
     * @param ruleEnum 规则
     * @param bizId    业务ID
     * @param content  备注
     */
    void grantExperience(String memberId, ExperienceRuleEnum ruleEnum, String bizId, String content);

    /**
     * 发放经验值（按金额倍率场景）
     *
     * @param memberId 客户ID
     * @param ruleEnum 规则
     * @param amount   金额
     * @param bizId    业务ID
     * @param content  备注
     */
    void grantExperienceByAmount(String memberId, ExperienceRuleEnum ruleEnum, Double amount, String bizId, String content);

    /**
     * 刷新客户等级
     *
     * @param memberId 客户ID
     */
    void refreshMemberGrade(String memberId);

    /**
     * 经验值分页列表
     *
     * @param mobile 客户手机号
     * @param ruleKey 规则编码
     * @param page 分页参数
     * @return 经验值分页数据
     */
    IPage<MemberExperienceLog> getExperiencePage(String mobile, String ruleKey, PageVO page);

    /**
     * 当前客户经验值分页列表（买家端）
     *
     * @param memberId 客户ID
     * @param ruleKey 规则编码
     * @param page 分页参数
     * @return 经验值分页数据
     */
    IPage<MemberExperienceLog> getExperiencePageByMemberId(String memberId, String ruleKey, PageVO page);

    /**
     * 获取已开启的经验值规则，并返回当前客户完成情况与已获得经验值
     *
     * @param memberId 客户ID
     * @return 经验值说明 + 规则列表
     */
    ExperienceRuleProgressListVO getEnabledRuleProgress(String memberId);
}
