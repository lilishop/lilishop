package cn.lili.controller.manager.distribution;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dto.DistributionMemberBatchGradeDTO;
import cn.lili.modules.distribution.entity.dto.DistributionMemberCreateDTO;
import cn.lili.modules.distribution.entity.dto.DistributionMemberUpdateDTO;
import cn.lili.modules.distribution.entity.dto.DistributionSearchParams;
import cn.lili.modules.distribution.entity.vos.DistributionMemberDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionMemberVO;
import cn.lili.modules.distribution.service.DistributionService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;

import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;

/**
 * 管理端,分销员管理接口
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@RestController
@Tag(name = "管理端,分销员管理接口")
@RequestMapping("/manager/distribution/distribution")
public class DistributionManagerController {

    @Autowired
    private DistributionService distributionService;

    @Operation(summary = "分页获取")
    @GetMapping("/getByPage")
    public ResultMessage<IPage<Distribution>> getByPage(DistributionSearchParams distributionSearchParams, PageVO page) {
        return ResultUtil.data(distributionService.distributionPage(distributionSearchParams, page));
    }

    @Operation(summary = "分销员会员分页(V2)")
    @GetMapping("/memberPage")
    public ResultMessage<IPage<DistributionMemberVO>> getMemberPage(DistributionSearchParams distributionSearchParams, PageVO page) {
        return ResultUtil.data(distributionService.getMemberPage(distributionSearchParams, page));
    }

    @Operation(summary = "导出分销员列表(V2)")
    @GetMapping(value = "/memberExport", produces = "application/octet-stream")
    public void exportMembers(DistributionSearchParams distributionSearchParams) {
        HttpServletResponse response = ThreadContextHolder.getHttpResponse();
        distributionService.exportMembers(response, distributionSearchParams);
    }

    @Operation(summary = "分销员详情(V2)")
    @GetMapping("/memberDetail/{id}")
    public ResultMessage<DistributionMemberDetailVO> getMemberDetail(
            @Parameter(description = "分销员id", required = true) @PathVariable String id) {
        return ResultUtil.data(distributionService.getMemberDetail(id));
    }


    @PreventDuplicateSubmissions
    @Operation(summary = "清退分销员")
    @PutMapping("/retreat/{id}")
    public ResultMessage<Object> retreat(
            @Parameter(description = "分销员id", required = true) @PathVariable String id) {
        if (distributionService.retreat(id)) {
            return ResultUtil.success();
        } else {
            throw new ServiceException(ResultCode.DISTRIBUTION_RETREAT_ERROR);
        }

    }

    @PreventDuplicateSubmissions
    @Operation(summary = "恢复分销员")
    @PutMapping("/resume/{id}")
    public ResultMessage<Object> resume(
            @Parameter(description = "分销员id", required = true) @PathVariable String id) {
        if (distributionService.resume(id)) {
            return ResultUtil.success();
        } else {
            throw new ServiceException(ResultCode.DISTRIBUTION_RETREAT_ERROR);
        }

    }

    @PreventDuplicateSubmissions
    @Operation(summary = "审核分销员")
    @PutMapping("/audit/{id}")
    public ResultMessage<Object> audit(
            @Parameter(description = "分销员id", required = true) @NotNull @PathVariable String id, 
            @Parameter(description = "审核结果，PASS 通过  REFUSE 拒绝", required = true) @NotNull String status) {
        if (distributionService.audit(id, status)) {
            return ResultUtil.success();
        } else {
            throw new ServiceException(ResultCode.DISTRIBUTION_AUDIT_ERROR);
        }
    }


    @PreventDuplicateSubmissions
    @Operation(summary = "新增分销员(V2)")
    @PostMapping("/member")
    public ResultMessage<DistributionMemberVO> createMember(@Valid @RequestBody DistributionMemberCreateDTO createDTO) {
        return ResultUtil.data(distributionService.createMember(createDTO));
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "编辑分销员(V2)")
    @PutMapping("/member/{id}")
    public ResultMessage<DistributionMemberVO> updateMember(
            @Parameter(description = "分销员id", required = true) @PathVariable String id,
            @Valid @RequestBody DistributionMemberUpdateDTO updateDTO) {
        return ResultUtil.data(distributionService.updateMember(id, updateDTO));
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "批量设置分销等级(V2)")
    @PutMapping("/member/batchGrade")
    public ResultMessage<Object> batchUpdateGrade(@Valid @RequestBody DistributionMemberBatchGradeDTO batchGradeDTO) {
        distributionService.batchUpdateGrade(batchGradeDTO);
        return ResultUtil.success();
    }

    @Operation(summary = "更新数据")
    @PutMapping("/{id}")
    public ResultMessage<Distribution> update(
            @Parameter(description = "分销员ID", required = true) @PathVariable String id, @Valid Distribution distribution) {
        distribution.setId(id);
        if (distributionService.updateById(distribution)) {
            return ResultUtil.data(distribution);
        }
        throw new ServiceException(ResultCode.DISTRIBUTION_EDIT_ERROR);
    }
}
