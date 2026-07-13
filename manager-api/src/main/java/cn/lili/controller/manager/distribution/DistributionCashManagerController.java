package cn.lili.controller.manager.distribution;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.DistributionCash;
import cn.lili.modules.distribution.entity.vos.DistributionCashSearchParams;
import cn.lili.modules.distribution.service.DistributionCashService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 管理端,分销佣金管理接口
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@RestController
@Tag(name = "管理端,分销佣金管理接口")
@RequestMapping("/manager/distribution/cash")
public class DistributionCashManagerController {

    @Autowired
    private DistributionCashService distributorCashService;

    @Operation(summary = "通过id获取分销佣金详情")
    @GetMapping("/get/{id}")
    public ResultMessage<DistributionCash> get(
            @Parameter(description = "分销佣金ID", required = true) @PathVariable String id) {
        return ResultUtil.data(distributorCashService.getById(id));
    }

    @Operation(summary = "分页获取")
    @GetMapping("/getByPage")
    public ResultMessage<IPage<DistributionCash>> getByPage(DistributionCashSearchParams distributionCashSearchParams) {

        return ResultUtil.data(distributorCashService.getDistributionCash(distributionCashSearchParams));
    }


    @PreventDuplicateSubmissions
    @Operation(summary = "审核")
    @PostMapping("/audit/{id}")
    public ResultMessage<DistributionCash> audit(
            @Parameter(description = "分销佣金ID", required = true) @PathVariable String id, 
            @Parameter(description = "处理结果", required = true) @NotNull String result) {
        return ResultUtil.data(distributorCashService.audit(id, result));
    }


    @Operation(summary = "查询分销提现导出列表")
    @GetMapping("/queryExport")
    public void queryExport(DistributionCashSearchParams distributionCashSearchParams) {
        HttpServletResponse response = ThreadContextHolder.getHttpResponse();
        distributorCashService.queryExport(response, distributionCashSearchParams);
    }
}

