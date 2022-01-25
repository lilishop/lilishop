package cn.lili.controller.distribution;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dto.DistributionSearchParams;
import cn.lili.modules.distribution.service.DistributionService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;

/**
 * 管理端,分销员管理接口
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@RestController
@Api(tags = "管理端,分销员管理接口")
@RequestMapping("/manager/distribution")
public class DistributionManagerController {

    @Autowired
    private DistributionService distributionService;

    @ApiOperation(value = "分页获取")
    @GetMapping(value = "/getByPage")
    public ResultMessage<IPage<Distribution>> getByPage(DistributionSearchParams distributionSearchParams, PageVO page) {
        return ResultUtil.data(distributionService.distributionPage(distributionSearchParams, page));
    }


    @PreventDuplicateSubmissions
    @ApiOperation(value = "清退分销商")
    @PutMapping(value = "/retreat/{id}")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "分销商id", required = true, paramType = "path", dataType = "String")
    })
    public ResultMessage<Object> retreat(@PathVariable String id) {
        if (distributionService.retreat(id)) {
            return ResultUtil.success();
        } else {
            throw new ServiceException(ResultCode.DISTRIBUTION_RETREAT_ERROR);
        }

    }

    @PreventDuplicateSubmissions
    @ApiOperation(value = "恢复分销商")
    @PutMapping(value = "/resume/{id}")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "分销商id", required = true, paramType = "path", dataType = "String")
    })
    public ResultMessage<Object> resume(@PathVariable String id) {
        if (distributionService.resume(id)) {
            return ResultUtil.success();
        } else {
            throw new ServiceException(ResultCode.DISTRIBUTION_RETREAT_ERROR);
        }

    }

    @PreventDuplicateSubmissions
    @ApiOperation(value = "审核分销商")
    @PutMapping(value = "/audit/{id}")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "分销商id", required = true, paramType = "path", dataType = "String"),
            @ApiImplicitParam(name = "status", value = "审核结果，PASS 通过  REFUSE 拒绝", required = true, paramType = "query", dataType = "String")
    })
    public ResultMessage<Object> audit(@NotNull @PathVariable String id, @NotNull String status) {
        if (distributionService.audit(id, status)) {
            return ResultUtil.success();
        } else {
            throw new ServiceException(ResultCode.DISTRIBUTION_AUDIT_ERROR);
        }

    }
}
