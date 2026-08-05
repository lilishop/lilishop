package cn.lili.controller.seller.distribution;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.dto.DistributionGoodsBatchCheckDTO;
import cn.lili.modules.distribution.entity.dto.DistributionGoodsSearchParams;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsVO;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import java.util.List;
import java.util.Objects;

/**
 * 店铺端,分销商品接口
 *
 * @author Bulbasaur
 * @since 2020/11/16 10:06 下午
 */
@RestController
@Tag(name = "店铺端,分销商品接口")
@RequestMapping("/store/distribution/goods")
public class DistributionGoodsStoreController {

    /**
     * 分销商品
     */
    @Autowired
    private DistributionGoodsService distributionGoodsService;

    @Operation(description = "获取分销员商品列表")
    @GetMapping
    public ResultMessage<IPage<DistributionGoodsVO>> distributionGoods(DistributionGoodsSearchParams distributionGoodsSearchParams) {

        return ResultUtil.data(distributionGoodsService.goodsPage(distributionGoodsSearchParams));
    }

    @Operation(description = "选择商品参与分销")
    @Parameter(name = "skuId", description = "规格ID", required = true)
    @PutMapping("/checked/{skuId}")
    public ResultMessage<DistributionGoods> distributionCheckGoods(@NotNull(message = "规格ID不能为空") @PathVariable String skuId,
                                                                   @NotNull(message = "佣金金额不能为空") @RequestParam Double commission) {

        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(distributionGoodsService.checked(skuId, commission, storeId));
    }

    @Operation(description = "批量选择商品参与分销")
    @PostMapping("/checked/batch")
    public ResultMessage<List<DistributionGoods>> distributionBatchCheckGoods(
            @Valid @RequestBody DistributionGoodsBatchCheckDTO batchDTO) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(distributionGoodsService.batchChecked(batchDTO, storeId));
    }

    @Operation(description = "修改分销商品佣金")
    @Parameter(name = "id", description = "分销商品ID", required = true)
    @PutMapping("/{id}/commission")
    public ResultMessage<DistributionGoods> updateCommission(@NotNull @PathVariable String id,
                                                             @NotNull(message = "佣金金额不能为空") @RequestParam Double commission) {
        DistributionGoods distributionGoods = OperationalJudgment.judgment(distributionGoodsService.getById(id));
        return ResultUtil.data(distributionGoodsService.updateCommission(distributionGoods, commission));
    }

    @Operation(description = "取消分销商品")
    @Parameter(name = "id", description = "分销员商品ID", required = true)
    @DeleteMapping("/cancel/{id}")
    public ResultMessage<Object> cancel(@NotNull @PathVariable String id) {
        OperationalJudgment.judgment(distributionGoodsService.getById(id));
        distributionGoodsService.removeById(id);
        return ResultUtil.success();
    }

}
