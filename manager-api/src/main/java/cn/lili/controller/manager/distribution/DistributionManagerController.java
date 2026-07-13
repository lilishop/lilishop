package cn.lili.controller.manager.distribution;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dto.DistributionSearchParams;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.goods.entity.vos.BrandVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;

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


    @PreventDuplicateSubmissions
    @Operation(summary = "清退分销商")
    @PutMapping("/retreat/{id}")
    public ResultMessage<Object> retreat(
            @Parameter(description = "分销商id", required = true) @PathVariable String id) {
        if (distributionService.retreat(id)) {
            return ResultUtil.success();
        } else {
            throw new ServiceException(ResultCode.DISTRIBUTION_RETREAT_ERROR);
        }

    }

    @PreventDuplicateSubmissions
    @Operation(summary = "恢复分销商")
    @PutMapping("/resume/{id}")
    public ResultMessage<Object> resume(
            @Parameter(description = "分销商id", required = true) @PathVariable String id) {
        if (distributionService.resume(id)) {
            return ResultUtil.success();
        } else {
            throw new ServiceException(ResultCode.DISTRIBUTION_RETREAT_ERROR);
        }

    }

    @PreventDuplicateSubmissions
    @Operation(summary = "审核分销商")
    @PutMapping("/audit/{id}")
    public ResultMessage<Object> audit(
            @Parameter(description = "分销商id", required = true) @NotNull @PathVariable String id, 
            @Parameter(description = "审核结果，PASS 通过  REFUSE 拒绝", required = true) @NotNull String status) {
        if (distributionService.audit(id, status)) {
            return ResultUtil.success();
        } else {
            throw new ServiceException(ResultCode.DISTRIBUTION_AUDIT_ERROR);
        }
    }


    @Operation(summary = "更新数据")
    @PutMapping("/{id}")
    public ResultMessage<Distribution> update(
            @Parameter(description = "分销商ID", required = true) @PathVariable String id, @Valid Distribution distribution) {
        distribution.setId(id);
        if (distributionService.updateById(distribution)) {
            return ResultUtil.data(distribution);
        }
        throw new ServiceException(ResultCode.DISTRIBUTION_EDIT_ERROR);
    }
}
