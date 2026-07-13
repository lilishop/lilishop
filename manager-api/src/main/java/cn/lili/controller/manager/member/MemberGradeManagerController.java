package cn.lili.controller.manager.member;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.entity.dos.MemberExperienceLog;
import cn.lili.modules.member.entity.dos.MemberGrade;
import cn.lili.modules.member.entity.vo.MemberGradeDetailVO;
import cn.lili.modules.member.entity.vo.MemberShareBuyLogVO;
import cn.lili.modules.member.entity.vo.MemberShareLogVO;
import cn.lili.modules.member.service.MemberBenefitService;
import cn.lili.modules.member.service.MemberExperienceService;
import cn.lili.modules.member.service.MemberGradeService;
import cn.lili.modules.member.service.MemberShareRegisterService;
import cn.lili.modules.member.service.MemberShareLogService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;

import java.util.Date;
import java.util.List;

/**
 * 管理端,客户等级接口
 *
 * @author Bulbasaur
 * @since 2021/5/16 11:29 下午
 */
@RestController
@Tag(name = "管理端,客户等级接口")
@RequestMapping("/manager/member/memberGrade")
public class MemberGradeManagerController {

    @Autowired
    private MemberGradeService memberGradeService;
    @Autowired
    private MemberBenefitService memberBenefitService;
    @Autowired
    private MemberExperienceService memberExperienceService;
    @Autowired
    private MemberShareLogService memberShareLogService;
    @Autowired
    private MemberShareRegisterService memberShareRegisterService;

    @Operation(description = "通过id获取客户等级详情（含关联客户权益列表）")
    @Parameter(name = "id", description = "客户等级ID", required = true)
    @GetMapping("/get/{id}")
    public ResultMessage<MemberGradeDetailVO> get(@PathVariable String id) {
        return ResultUtil.data(memberGradeService.getManagerGradeDetail(id));
    }

    @Operation(description = "获取客户等级列表")
    @GetMapping("/getByPage")
    public ResultMessage<List<MemberGrade>> getByPage() {
        return ResultUtil.data(memberGradeService.listForUpgrade());
    }

    @Operation(description = "经验值分页列表")
    @Parameter(name = "mobile", description = "客户手机号", required = false)
    @Parameter(name = "ruleKey", description = "规则编码", required = false)
    @Parameter(name = "page", description = "分页参数", required = false)
    @GetMapping("/experience/getByPage")
    public ResultMessage<IPage<MemberExperienceLog>> getExperienceByPage(@RequestParam(required = false) String mobile,
                                                                         @RequestParam(required = false) String ruleKey,
                                                                         PageVO page) {
        return ResultUtil.data(memberExperienceService.getExperiencePage(mobile, ruleKey, page));
    }

    @Operation(description = "会员分享记录分页列表")
    @Parameter(name = "mobile", description = "客户手机号", required = false)
    @Parameter(name = "startTime", description = "开始时间", required = false)
    @Parameter(name = "endTime", description = "结束时间", required = false)
    @Parameter(name = "page", description = "分页参数", required = false)
    @GetMapping("/share/getByPage")
    public ResultMessage<IPage<MemberShareLogVO>> getShareLogByPage(@RequestParam(required = false) String mobile,
                                                                     @RequestParam(required = false) @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss") Date startTime,
                                                                     @RequestParam(required = false) @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss") Date endTime,
                                                                     PageVO page) {
        return ResultUtil.data(memberShareLogService.getShareLogPage(mobile, startTime, endTime, page));
    }

    @Operation(description = "分享购买奖励记录分页列表")
    @Parameter(name = "mobile", description = "客户手机号", required = false)
    @Parameter(name = "startTime", description = "开始时间", required = false)
    @Parameter(name = "endTime", description = "结束时间", required = false)
    @Parameter(name = "page", description = "分页参数", required = false)
    @GetMapping("/share/buy/getByPage")
    public ResultMessage<IPage<MemberShareBuyLogVO>> getShareBuyLogByPage(@RequestParam(required = false) String mobile,
                                                                           @RequestParam(required = false) @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss") Date startTime,
                                                                           @RequestParam(required = false) @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss") Date endTime,
                                                                           PageVO page) {
        return ResultUtil.data(memberShareRegisterService.getShareBuyLogPage(mobile, startTime, endTime, page));
    }

    @Operation(description = "添加客户等级")
    @Parameter(name = "memberGrade", description = "客户等级")
    @PostMapping
    public ResultMessage<Object> add(@Valid MemberGrade memberGrade) {
        memberGradeService.saveGrade(memberGrade);
        return ResultUtil.success(ResultCode.SUCCESS);
    }

    @Operation(description = "修改客户等级")
    @Parameter(name = "id", description = "客户等级ID", required = true)
    @Parameter(name = "memberGrade", description = "客户等级")
    @PutMapping("/update/{id}")
    public ResultMessage<Object> update(@PathVariable String id, @Valid MemberGrade memberGrade) {
        memberGradeService.updateGrade(id, memberGrade);
        return ResultUtil.success(ResultCode.SUCCESS);
    }

    @Operation(description = "删除客户等级")
    @Parameter(name = "id", description = "客户等级ID", required = true)
    @DeleteMapping("/delete/{id}")
    public ResultMessage<Object> delete(@PathVariable String id) {
        memberGradeService.deleteGrade(id);
        return ResultUtil.success(ResultCode.SUCCESS);
    }

    @Operation(description = "切换客户等级开关")
    @Parameter(name = "id", description = "客户等级ID", required = true)
    @Parameter(name = "state", description = "状态 OPEN/CLOSE", required = true)
    @PutMapping("/state/{id}")
    public ResultMessage<Object> updateState(@PathVariable String id, @RequestParam String state) {
        memberGradeService.updateGradeState(id, state);
        return ResultUtil.success(ResultCode.SUCCESS);
    }

    @Operation(description = "获取客户等级权益列表")
    @Parameter(name = "gradeId", description = "客户等级ID", required = true)
    @GetMapping("/{gradeId}/benefits")
    public ResultMessage<List<MemberBenefit>> getBenefits(@PathVariable String gradeId) {
        List<String> ids = memberGradeService.getGradeBenefitIdList(gradeId);
        return ResultUtil.data(memberBenefitService.listByIdList(ids, false));
    }

    @Operation(description = "设置客户等级权益")
    @Parameter(name = "gradeId", description = "客户等级ID", required = true)
    @Parameter(name = "benefitIds", description = "权益ID列表", required = false)
    @PutMapping("/{gradeId}/benefits")
    public ResultMessage<Object> setBenefits(@PathVariable String gradeId, @RequestParam(required = false) List<String> benefitIds) {
        memberGradeService.updateGradeBenefits(gradeId, benefitIds);
        return ResultUtil.success(ResultCode.SUCCESS);
    }
}
