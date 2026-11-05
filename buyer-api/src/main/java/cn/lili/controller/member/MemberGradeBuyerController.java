package cn.lili.controller.member;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberExperienceLog;
import cn.lili.modules.member.entity.dos.MemberGrade;
import cn.lili.modules.member.entity.vo.ExperienceRuleProgressListVO;
import cn.lili.modules.member.entity.vo.MemberGradeDetailVO;
import cn.lili.modules.member.service.MemberExperienceService;
import cn.lili.modules.member.service.MemberGradeService;
import cn.lili.modules.member.service.MemberService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * 买家端,客户等级接口
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@RestController
@Tag(name = "买家端,客户等级接口")
@RequestMapping("/buyer/member/memberGrade")
public class MemberGradeBuyerController {

    @Autowired
    private MemberService memberService;
    @Autowired
    private MemberGradeService memberGradeService;
    @Autowired
    private MemberExperienceService memberExperienceService;

    @Operation(summary = "获取当前客户等级信息与经验值记录")
    @Parameter(name = "ruleKey", description = "规则编码(可选)", required = false)
    @Parameter(name = "page", description = "分页参数", required = false)
    @GetMapping("/current")
    public ResultMessage<Map<String, Object>> current(String ruleKey, PageVO page) {
        String memberId = Objects.requireNonNull(UserContext.getCurrentUser()).getId();
        Member member = memberService.getById(memberId);
        if (member == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        MemberGrade grade = null;
        if (CharSequenceUtil.isNotBlank(member.getGradeId())) {
            grade = memberGradeService.getById(member.getGradeId());
        }
        if (grade == null || !"OPEN".equals(grade.getGradeState())) {
            grade = memberGradeService.getCurrentGradeByExperience(member.getExperience());
        }
        IPage<MemberExperienceLog> experiencePage = memberExperienceService.getExperiencePageByMemberId(memberId, ruleKey, page);

        Map<String, Object> result = new HashMap<>(4);
        result.put("grade", grade);
        result.put("experience", member.getExperience());
        result.put("gradeId", grade == null ? null : grade.getId());
        result.put("experienceLogs", experiencePage);
        return ResultUtil.data(result);
    }

    @Operation(summary = "获取经验值获取方式列表")
    @GetMapping("/rules")
    public ResultMessage<ExperienceRuleProgressListVO> rules() {
        String memberId = Objects.requireNonNull(UserContext.getCurrentUser()).getId();
        return ResultUtil.data(memberExperienceService.getEnabledRuleProgress(memberId));
    }

    @Operation(summary = "获取客户等级列表（每项含关联权益信息）")
    @GetMapping("/list")
    public ResultMessage<List<MemberGradeDetailVO>> list() {
        return ResultUtil.data(memberGradeService.listForUpgradeWithBenefits());
    }
}
