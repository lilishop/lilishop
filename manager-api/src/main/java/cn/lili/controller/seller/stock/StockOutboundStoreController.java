package cn.lili.controller.seller.stock;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.stock.entity.dos.StockOutbound;
import cn.lili.modules.stock.entity.dto.StockOutboundAuditDTO;
import cn.lili.modules.stock.entity.dto.StockOutboundCreateDTO;
import cn.lili.modules.stock.entity.params.StockOutboundSearchParams;
import cn.lili.modules.stock.entity.vos.StockOutboundVO;
import cn.lili.modules.stock.service.StockOutboundService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

/**
 * 商家端出库单控制器
 * 提供手工出库单的分页查询、详情查看、草稿创建、编辑、提交审核、审核与作废接口
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Tag(name = "店铺端,出库单接口")
@RestController
@RequestMapping("/store/stock/outbound")
public class StockOutboundStoreController {

    @Autowired
    private StockOutboundService stockOutboundService;

    @Operation(summary = "出库单分页列表")
    @GetMapping
    public ResultMessage<IPage<StockOutbound>> page(StockOutboundSearchParams params) {
        AuthUser user = Objects.requireNonNull(UserContext.getCurrentUser());
        params.setStoreId(user.getStoreId());
        return ResultUtil.data(stockOutboundService.page(params));
    }

    @Operation(summary = "出库单详情（含明细）")
    @Parameter(name = "id", description = "出库单ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<StockOutboundVO> detail(@PathVariable String id) {
        return ResultUtil.data(stockOutboundService.getDetail(id));
    }

    @Operation(summary = "创建出库单（保存草稿）")
    @PostMapping
    public ResultMessage<StockOutboundVO> create(@Valid @RequestBody StockOutboundCreateDTO dto) {
        return ResultUtil.data(stockOutboundService.createDraft(dto));
    }

    @Operation(summary = "编辑出库单（草稿或已驳回）")
    @Parameter(name = "id", description = "出库单ID", required = true)
    @PutMapping("/{id}")
    public ResultMessage<StockOutboundVO> update(@PathVariable String id, @Valid @RequestBody StockOutboundCreateDTO dto) {
        return ResultUtil.data(stockOutboundService.updateDraft(id, dto));
    }

    @Operation(summary = "提交出库单审核")
    @Parameter(name = "id", description = "出库单ID", required = true)
    @PutMapping("/submit/{id}")
    public ResultMessage<Boolean> submit(@PathVariable String id) {
        return ResultUtil.data(stockOutboundService.submit(id));
    }

    @Operation(summary = "审核出库单")
    @Parameter(name = "id", description = "出库单ID", required = true)
    @PutMapping("/audit/{id}")
    public ResultMessage<Boolean> audit(@PathVariable String id, @Valid @RequestBody StockOutboundAuditDTO dto) {
        return ResultUtil.data(stockOutboundService.audit(id, dto));
    }

    @Operation(summary = "作废出库单")
    @Parameter(name = "id", description = "出库单ID", required = true)
    @PutMapping("/cancel/{id}")
    public ResultMessage<Boolean> cancel(@PathVariable String id) {
        return ResultUtil.data(stockOutboundService.cancel(id));
    }
}
