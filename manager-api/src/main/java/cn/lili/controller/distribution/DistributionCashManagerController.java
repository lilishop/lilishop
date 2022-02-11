package cn.lili.controller.distribution;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.DistributionCash;
import cn.lili.modules.distribution.entity.vos.DistributionCashSearchParams;
import cn.lili.modules.distribution.service.DistributionCashService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;

/**
 * 管理端,分销佣金管理接口
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@RestController
@Api(tags = "管理端,分销佣金管理接口")
@RequestMapping("/manager/distribution/cash")
public class DistributionCashManagerController {

    @Autowired
    private DistributionCashService distributorCashService;

    @ApiOperation(value = "通过id获取分销佣金详情")
    @GetMapping(value = "/get/{id}")
    public ResultMessage<DistributionCash> get(@PathVariable String id) {
        return ResultUtil.data(distributorCashService.getById(id));
    }

    @ApiOperation(value = "分页获取")
    @GetMapping(value = "/getByPage")
    public ResultMessage<IPage<DistributionCash>> getByPage(DistributionCashSearchParams distributionCashSearchParams) {

        return ResultUtil.data(distributorCashService.getDistributionCash(distributionCashSearchParams));
    }


    @PreventDuplicateSubmissions
    @ApiOperation(value = "审核")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "分销佣金ID", required = true, paramType = "path", dataType = "String"),
            @ApiImplicitParam(name = "result", value = "处理结果", required = true, paramType = "query", dataType = "String")
    })
    @PostMapping(value = "/audit/{id}")
    public ResultMessage<DistributionCash> audit(@PathVariable String id, @NotNull String result) {
        return ResultUtil.data(distributorCashService.audit(id, result));
    }
}

