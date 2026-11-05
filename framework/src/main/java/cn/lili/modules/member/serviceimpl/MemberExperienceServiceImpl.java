package cn.lili.modules.member.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberExperienceLog;
import cn.lili.modules.member.entity.dos.MemberGrade;
import cn.lili.modules.member.entity.enums.ExperienceRuleEnum;
import cn.lili.modules.member.entity.vo.ExperienceRuleProgressListVO;
import cn.lili.modules.member.entity.vo.ExperienceRuleProgressVO;
import cn.lili.modules.member.mapper.MemberExperienceLogMapper;
import cn.lili.modules.member.service.MemberBenefitService;
import cn.lili.modules.member.service.MemberExperienceService;
import cn.lili.modules.member.service.MemberGradeService;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.ExperienceSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.gson.Gson;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 客户经验值服务实现
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Slf4j
@Service
public class MemberExperienceServiceImpl implements MemberExperienceService {

    @Autowired
    private SettingService settingService;
    @Autowired
    private MemberService memberService;
    @Autowired
    private MemberGradeService memberGradeService;
    @Autowired
    private MemberBenefitService memberBenefitService;
    @Autowired
    private MemberExperienceLogMapper memberExperienceLogMapper;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void grantExperience(String memberId, ExperienceRuleEnum ruleEnum, String bizId, String content) {
        this.grant(memberId, ruleEnum, null, bizId, content);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void grantExperienceByAmount(String memberId, ExperienceRuleEnum ruleEnum, Double amount, String bizId, String content) {
        this.grant(memberId, ruleEnum, amount, bizId, content);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void refreshMemberGrade(String memberId) {
        Member member = memberService.getById(memberId);
        if (member == null) {
            return;
        }
        MemberGrade computed = memberGradeService.getCurrentGradeByExperience(member.getExperience());
        if (computed == null) {
            return;
        }
        MemberGrade storedGrade = null;
        if (CharSequenceUtil.isNotBlank(member.getGradeId())) {
            storedGrade = memberGradeService.getById(member.getGradeId());
        }
        MemberGrade finalGrade = computed;
        if (storedGrade != null && "OPEN".equals(storedGrade.getGradeState())) {
            Integer se = storedGrade.getRequiredExperience();
            Integer ce = computed.getRequiredExperience();
            if (se != null && ce != null && se > ce) {
                finalGrade = storedGrade;
            }
        }
        String oldGradeId = member.getGradeId();
        String newGradeId = finalGrade.getId();
        if (CharSequenceUtil.equals(oldGradeId, newGradeId)) {
            this.memberBenefitService.tryGrantBenefitsForGradeReached(memberId, newGradeId);
            return;
        }
        member.setGradeId(newGradeId);
        memberService.updateById(member);
        this.memberBenefitService.tryGrantBenefitsForGradeReached(memberId, newGradeId);
    }

    @Override
    public IPage<MemberExperienceLog> getExperiencePage(String mobile, String ruleKey, PageVO page) {
        QueryWrapper<MemberExperienceLog> wrapper = new QueryWrapper<>();
        if (CharSequenceUtil.isNotBlank(mobile)) {
            Member member = memberService.findByMobile(mobile);
            if (member == null) {
                wrapper.eq("el.member_id", "__NOT_EXIST_MEMBER__");
            } else {
                wrapper.eq("el.member_id", member.getId());
            }
        }
        wrapper.eq(CharSequenceUtil.isNotBlank(ruleKey), "el.rule_key", ruleKey);
        wrapper.orderByDesc("el.create_time");
        return memberExperienceLogMapper.pageWithMember(PageUtil.initPage(page), wrapper);
    }

    @Override
    public IPage<MemberExperienceLog> getExperiencePageByMemberId(String memberId, String ruleKey, PageVO page) {
        if (CharSequenceUtil.isBlank(memberId)) {
            return PageUtil.initPage(page);
        }
        QueryWrapper<MemberExperienceLog> wrapper = new QueryWrapper<>();
        wrapper.eq("el.member_id", memberId);
        wrapper.eq(CharSequenceUtil.isNotBlank(ruleKey), "el.rule_key", ruleKey);
        wrapper.orderByDesc("el.create_time");
        return memberExperienceLogMapper.pageWithMember(PageUtil.initPage(page), wrapper);
    }

    @Override
    public ExperienceRuleProgressListVO getEnabledRuleProgress(String memberId) {
        if (CharSequenceUtil.isBlank(memberId)) {
            ExperienceRuleProgressListVO empty = new ExperienceRuleProgressListVO();
            empty.setItems(List.of());
            return empty;
        }
        Setting setting = settingService.get(SettingEnum.EXPERIENCE_SETTING.name());
        if (setting == null || CharSequenceUtil.isBlank(setting.getSettingValue())) {
            ExperienceRuleProgressListVO empty = new ExperienceRuleProgressListVO();
            empty.setItems(List.of());
            return empty;
        }
        ExperienceSetting experienceSetting = new Gson().fromJson(setting.getSettingValue(), ExperienceSetting.class);
        if (experienceSetting == null || experienceSetting.getItems() == null) {
            ExperienceRuleProgressListVO empty = new ExperienceRuleProgressListVO();
            empty.setItems(List.of());
            return empty;
        }

        List<ExperienceRuleProgressVO> result = new ArrayList<>();
        for (ExperienceSetting.ExperienceRuleItem item : experienceSetting.getItems()) {
            if (item == null || !Boolean.TRUE.equals(item.getEnabled()) || CharSequenceUtil.isBlank(item.getRuleKey())) {
                continue;
            }
            String ruleKey = item.getRuleKey();
            Long sum = memberExperienceLogMapper.sumByMemberAndRule(memberId, ruleKey);
            long gained = sum == null ? 0L : sum;
            Long count = memberExperienceLogMapper.countByMemberAndRule(memberId, ruleKey);
            boolean completed = (count != null && count > 0) || gained > 0L;

            ExperienceRuleProgressVO row = new ExperienceRuleProgressVO();
            row.setRuleKey(ruleKey);
            row.setRuleName(item.getRuleName());
            row.setValue(item.getValue());
            row.setMaxValue(item.getMaxValue());
            row.setCompleted(completed);
            row.setGainedExperience(gained);
            result.add(row);
        }
        ExperienceRuleProgressListVO vo = new ExperienceRuleProgressListVO();
        vo.setDescription(experienceSetting.getDescription());
        vo.setItems(result);
        return vo;
    }

    /**
     * 按开启的经验规则发放经验，查询规则及业务记录防重，并写入经验流水后刷新客户等级。
     * 新增地址先锁客户行；一次性任务按客户和规则、签到按业务日期、消费及分享等按业务ID查询去重。
     * 除消费外的规则按该客户累计已得经验限制maxValue，经验余额使用SQL原子累加。
     *
     * @param memberId 客户主键，空值不处理
     * @param ruleEnum 触发的经验规则，空值不处理
     * @param amount   消费金额，单位元，仅消费规则使用
     * @param bizId    订单号、被邀请人、店铺ID或签到日期等业务防重标识
     * @param content  写入经验流水的变更说明
     * @throws ServiceException 客户不存在
     */
    private void grant(String memberId, ExperienceRuleEnum ruleEnum, Double amount, String bizId, String content) {
        if (memberId == null || ruleEnum == null) {
            return;
        }
        Member member;
        if (ruleEnum == ExperienceRuleEnum.ADD_ADDRESS) {
            QueryWrapper<Member> lockWrapper = new QueryWrapper<>();
            lockWrapper.eq("id", memberId);
            lockWrapper.last("FOR UPDATE");
            member = memberService.getOne(lockWrapper);
        } else {
            member = memberService.getById(memberId);
        }
        if (member == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        // 所有经验值发放都遵循对应规则的开启状态
        ExperienceSetting.ExperienceRuleItem ruleItem = this.getRuleItem(ruleEnum);
        if (ruleItem == null || !Boolean.TRUE.equals(ruleItem.getEnabled())) {
            return;
        }
        // 注册经验值仅赠送一次
        if (ruleEnum == ExperienceRuleEnum.REGISTER) {
            Long count = memberExperienceLogMapper.countByMemberAndRule(memberId, ruleEnum.name());
            if (count != null && count > 0) {
                return;
            }
        }
        // 完善个人信息经验值仅赠送一次
        if (ruleEnum == ExperienceRuleEnum.PROFILE) {
            Long count = memberExperienceLogMapper.countByMemberAndRule(memberId, ruleEnum.name());
            if (count != null && count > 0) {
                return;
            }
        }
        // 绑定微信经验值仅赠送一次
        if (ruleEnum == ExperienceRuleEnum.BIND_WECHAT) {
            Long count = memberExperienceLogMapper.countByMemberAndRule(memberId, ruleEnum.name());
            if (count != null && count > 0) {
                return;
            }
        }
        if (ruleEnum == ExperienceRuleEnum.ADD_ADDRESS) {
            Long count = memberExperienceLogMapper.countByMemberAndRule(memberId, ruleEnum.name());
            if (count != null && count > 0) {
                return;
            }
        }
        // 签到经验值每天仅赠送一次（按 会员+规则+业务日期 幂等）
        if (ruleEnum == ExperienceRuleEnum.SIGN_IN) {
            String signBizId = CharSequenceUtil.isBlank(bizId) ? DateUtil.toString(new Date(), DateUtil.STANDARD_DATE_NO_UNDERLINE_FORMAT) : bizId;
            Long count = memberExperienceLogMapper.countByMemberRuleAndBiz(memberId, ruleEnum.name(), signBizId);
            if (count != null && count > 0) {
                return;
            }
            bizId = signBizId;
        }
        // 业务幂等：订单完成按 会员+规则+业务ID 去重
        if (ruleEnum == ExperienceRuleEnum.CONSUME && CharSequenceUtil.isNotBlank(bizId)) {
            Long count = memberExperienceLogMapper.countByMemberRuleAndBiz(memberId, ruleEnum.name(), bizId);
            if (count != null && count > 0) {
                return;
            }
        }
        // 分享注册奖励按 分享人+规则+被邀请人(业务ID) 幂等，避免重复发放
        if (ruleEnum == ExperienceRuleEnum.SHARE_REGISTER && CharSequenceUtil.isNotBlank(bizId)) {
            Long count = memberExperienceLogMapper.countByMemberRuleAndBiz(memberId, ruleEnum.name(), bizId);
            if (count != null && count > 0) {
                return;
            }
        }
        // 分享购买奖励按 分享人+规则+订单号(业务ID) 幂等，避免重复发放
        if (ruleEnum == ExperienceRuleEnum.SHARE_BUY && CharSequenceUtil.isNotBlank(bizId)) {
            Long count = memberExperienceLogMapper.countByMemberRuleAndBiz(memberId, ruleEnum.name(), bizId);
            if (count != null && count > 0) {
                return;
            }
        }
        // 关注店铺奖励按 会员+规则+店铺ID(业务ID) 幂等，避免重复发放
        if (ruleEnum == ExperienceRuleEnum.FOLLOW_STORE && CharSequenceUtil.isNotBlank(bizId)) {
            Long count = memberExperienceLogMapper.countByMemberRuleAndBiz(memberId, ruleEnum.name(), bizId);
            if (count != null && count > 0) {
                return;
            }
        }

        long variableExperience = this.calcVariableExperience(ruleEnum, amount, ruleItem);
        if (variableExperience <= 0L) {
            return;
        }

        if (ruleEnum != ExperienceRuleEnum.CONSUME) {
            if (ruleItem.getMaxValue() != null) {
                Long total = memberExperienceLogMapper.sumByMemberAndRule(memberId, ruleEnum.name());
                long currentValue = total == null ? 0L : total;
                long canGet = ruleItem.getMaxValue() - currentValue;
                if (canGet <= 0L) {
                    return;
                }
                variableExperience = Math.min(variableExperience, canGet);
            }
        }

        // 原子累加经验值，避免并发下丢失更新
        UpdateWrapper<Member> updateWrapper = Wrappers.update();
        updateWrapper.eq("id", memberId);
        updateWrapper.setSql("experience = IFNULL(experience,0) + " + variableExperience);
        memberService.update(updateWrapper);

        Member latestMember = memberService.getById(memberId);
        long currentExperience = latestMember.getExperience() == null ? 0L : latestMember.getExperience();
        long beforeExperience = Math.max(currentExperience - variableExperience, 0L);

        MemberExperienceLog logItem = new MemberExperienceLog();
        logItem.setMemberId(memberId);
        logItem.setMemberName(member.getUsername());
        logItem.setRuleKey(ruleEnum.name());
        logItem.setBizId(bizId);
        logItem.setBeforeExperience(beforeExperience);
        logItem.setVariableExperience(variableExperience);
        logItem.setExperience(currentExperience);
        logItem.setContent(content);
        memberExperienceLogMapper.insert(logItem);

        refreshMemberGrade(memberId);
    }

    /**
     * 计算本次经验增量，消费按金额向下取整后乘每元奖励，其他规则取配置固定奖励。
     * 累计上限由grant方法处理，本方法不扣减已获得的经验。
     *
     * @param ruleEnum 经验规则
     * @param amount   消费金额，单位元，消费规则下非正数或空值不发放
     * @param ruleItem 已读取的规则配置
     * @return 本次应增加的经验值，奖励未配置或非正数时为0
     */
    private long calcVariableExperience(ExperienceRuleEnum ruleEnum, Double amount, ExperienceSetting.ExperienceRuleItem ruleItem) {
        if (ruleEnum == ExperienceRuleEnum.CONSUME) {
            if (amount == null || amount <= 0) {
                return 0L;
            }
            if (ruleItem.getValue() == null || ruleItem.getValue() <= 0) {
                return 0L;
            }
            // 消费经验值规则：每 1 元赠送 value 点经验值，按金额向下取整后计算
            long floorAmount = BigDecimal.valueOf(amount).setScale(0, RoundingMode.DOWN).longValue();
            return floorAmount * ruleItem.getValue();
        }
        if (ruleItem.getValue() == null || ruleItem.getValue() <= 0) {
            return 0L;
        }
        return ruleItem.getValue();
    }

    /**
     * 从li_setting的EXPERIENCE_SETTING中按规则编码读取配置，不在此过滤开启状态。
     *
     * @param ruleEnum 需要查询的经验规则
     * @return 对应配置，设置缺失、内容空白或未找到规则时为null
     */
    private ExperienceSetting.ExperienceRuleItem getRuleItem(ExperienceRuleEnum ruleEnum) {
        Setting setting = settingService.get(SettingEnum.EXPERIENCE_SETTING.name());
        if (setting == null || CharSequenceUtil.isBlank(setting.getSettingValue())) {
            return null;
        }
        ExperienceSetting experienceSetting = new Gson().fromJson(setting.getSettingValue(), ExperienceSetting.class);
        if (experienceSetting == null || experienceSetting.getItems() == null) {
            return null;
        }
        for (ExperienceSetting.ExperienceRuleItem item : experienceSetting.getItems()) {
            if (item != null && CharSequenceUtil.equals(ruleEnum.name(), item.getRuleKey())) {
                return item;
            }
        }
        return null;
    }
}
