package cn.lili.controller.manager.order;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.payment.entity.RefundLog;
import cn.lili.modules.payment.service.RefundLogService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,退款日志接口
 *
 * @author Chopper
 * @since 2020/11/16 10:07 下午
 */
@RestController
@Tag(name = "管理端,退款日志接口")
@RequestMapping("/manager/order/refundLog")
public class RefundLogManagerController {
    @Autowired
    private RefundLogService refundLogService;

    @Operation(description = "查看退款日志详情")
    @Parameter(name = "id", description = "退款日志ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<RefundLog> get(@PathVariable String id) {
        return ResultUtil.data(refundLogService.getById(id));
    }

    @Operation(description = "分页获取退款日志")
    @Parameter(name = "entity", description = "查询参数")
    @Parameter(name = "searchVo", description = "分页参数")
    @Parameter(name = "page", description = "分页参数")
    @GetMapping
    public ResultMessage<IPage<RefundLog>> getByPage(RefundLog entity, SearchVO searchVo, PageVO page) {
        return ResultUtil.data(refundLogService.getByPage(entity, searchVo, page));
    }

}
