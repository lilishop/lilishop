package cn.lili.controller.seller.stock;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.stock.entity.dos.StockFlow;
import cn.lili.modules.stock.entity.params.StockFlowSearchParams;
import cn.lili.modules.stock.service.StockFlowService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;

/**
 * 商家端库存流水控制器
 * 提供按商品、SKU、变动类型、方向及时间范围的分页台账查询
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Tag(name = "店铺端,库存流水接口")
@RestController
@RequestMapping("/store/stock/flow")
public class StockFlowStoreController {

    @Autowired
    private StockFlowService stockFlowService;

    @Operation(summary = "库存流水分页列表")
    @GetMapping
    public ResultMessage<IPage<StockFlow>> page(StockFlowSearchParams params) {
        AuthUser user = Objects.requireNonNull(UserContext.getCurrentUser());
        params.setStoreId(user.getStoreId());
        return ResultUtil.data(stockFlowService.page(params));
    }
}
