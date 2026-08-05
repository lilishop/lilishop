package cn.lili.controller.manager.distribution;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dto.DistributionCustomerRelationSearchParams;
import cn.lili.modules.distribution.entity.vos.DistributionCustomerRelationDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionCustomerRelationLogVO;
import cn.lili.modules.distribution.entity.vos.DistributionCustomerRelationVO;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 管理端,分销客户关系接口
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@RestController
@Tag(name = "管理端,分销客户关系接口")
@RequestMapping("/manager/distribution/customerRelation")
public class DistributionCustomerRelationManagerController {

    @Autowired
    private DistributionCustomerRelationService customerRelationService;

    @Operation(summary = "客户关系分页")
    @GetMapping
    public ResultMessage<IPage<DistributionCustomerRelationVO>> page(DistributionCustomerRelationSearchParams params, PageVO page) {
        return ResultUtil.data(customerRelationService.relationPage(params, page));
    }

    @Operation(summary = "客户关系详情")
    @GetMapping("/{id}")
    public ResultMessage<DistributionCustomerRelationDetailVO> detail(@PathVariable String id) {
        return ResultUtil.data(customerRelationService.getRelationDetail(id));
    }

    @Operation(summary = "客户关系变更记录")
    @GetMapping("/{id}/logs")
    public ResultMessage<List<DistributionCustomerRelationLogVO>> logs(@PathVariable String id) {
        return ResultUtil.data(customerRelationService.listRelationLogs(id));
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "后台换绑")
    @PutMapping("/rebind")
    public ResultMessage<Object> rebind(@RequestParam String memberId,
                                        @RequestParam String distributionId,
                                        @RequestParam(required = false) String reason) {
        customerRelationService.adminRebind(memberId, distributionId, reason, null);
        return ResultUtil.success();
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "后台解绑")
    @PutMapping("/unbind")
    public ResultMessage<Object> unbind(@RequestParam String memberId,
                                        @RequestParam(required = false) String reason) {
        customerRelationService.adminUnbind(memberId, reason, null);
        return ResultUtil.success();
    }
}
