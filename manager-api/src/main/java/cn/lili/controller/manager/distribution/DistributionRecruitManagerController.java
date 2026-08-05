package cn.lili.controller.manager.distribution;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.DistributionRecruitApplication;
import cn.lili.modules.distribution.service.DistributionRecruitService;
import cn.lili.modules.system.entity.dto.DistributionRecruitSetting;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 管理端,分销招募接口
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@RestController
@Tag(name = "管理端,分销招募接口")
@RequestMapping("/manager/distribution/recruit")
public class DistributionRecruitManagerController {

    @Autowired
    private DistributionRecruitService recruitService;

    @Operation(summary = "获取当前分销招募设置")
    @GetMapping("/plan/current")
    public ResultMessage<DistributionRecruitSetting> currentPlan() {
        return ResultUtil.data(recruitService.getSetting());
    }

    @Operation(summary = "获取已发布分销招募设置")
    @GetMapping("/plan/published")
    public ResultMessage<DistributionRecruitSetting> publishedPlan() {
        return ResultUtil.data(recruitService.getSetting());
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "保存分销招募设置")
    @PostMapping("/plan")
    public ResultMessage<DistributionRecruitSetting> savePlan(@RequestBody DistributionRecruitSetting setting) {
        return ResultUtil.data(recruitService.saveSetting(setting));
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "发布分销招募设置（兼容旧接口，等同保存）")
    @PutMapping("/plan/publish/{planId}")
    public ResultMessage<DistributionRecruitSetting> publish(@PathVariable String planId) {
        return ResultUtil.data(recruitService.getSetting());
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "审核招募申请")
    @PutMapping("/application/audit/{applicationId}")
    public ResultMessage<Object> audit(@PathVariable String applicationId,
                                       @RequestParam boolean pass,
                                       @RequestParam(required = false) String remark) {
        recruitService.auditApplication(applicationId, pass, remark, null);
        return ResultUtil.success();
    }
}
