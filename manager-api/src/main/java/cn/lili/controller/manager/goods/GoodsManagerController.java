package cn.lili.controller.manager.goods;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsVirtualSalesDTO;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.vos.GoodsNumVO;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.NotEmpty;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 管理端,商品管理接口
 *
 * @author pikachu
 * @since 2020-02-23 15:18:56
 */
@RestController
@Validated
@Tag(name = "管理端,商品管理接口")
@RequestMapping("/manager/goods/goods")
public class GoodsManagerController {
    /**
     * 商品
     */
    @Autowired
    private GoodsService goodsService;
    /**
     * 规格商品
     */
    @Autowired
    private GoodsSkuService goodsSkuService;

    @Operation(summary = "分页获取")
    @Parameter(name = "goodsSearchParams", description = "商品查询参数")
    @GetMapping("/list")
    public ResultMessage<IPage<Goods>> getByPage(GoodsSearchParams goodsSearchParams) {
        return ResultUtil.data(goodsService.queryByParams(goodsSearchParams));
    }

    @Operation(summary = "获取商品数量")
    @Parameter(name = "goodsSearchParams", description = "商品查询参数")
    @GetMapping("/goodsNumber")
    public ResultMessage<GoodsNumVO> getGoodsNumVO(GoodsSearchParams goodsSearchParams) {
        return ResultUtil.data(goodsService.getGoodsNumVO(goodsSearchParams));
    }

    @Operation(summary = "分页获取规格列表")
    @Parameter(name = "goodsSearchParams", description = "商品查询参数")
    @GetMapping("/sku/list")
    public ResultMessage<IPage<GoodsSku>> getSkuByPage(GoodsSearchParams goodsSearchParams) {
        goodsSearchParams.setSort("create_time");
        goodsSearchParams.setOrder("desc");
        return ResultUtil.data(goodsSkuService.getGoodsSkuByPage(goodsSearchParams));
    }

    @Operation(summary = "分页获取待审核商品")
    @Parameter(name = "goodsSearchParams", description = "商品查询参数")
    @GetMapping("/auth/list")
    public ResultMessage<IPage<Goods>> getAuthPage(GoodsSearchParams goodsSearchParams) {
        goodsSearchParams.setAuthFlag(GoodsAuthEnum.TOBEAUDITED.name());
        return ResultUtil.data(goodsService.queryByParams(goodsSearchParams));
    }

    @PreventDuplicateSubmissions
    @Operation(description = "管理员审核商品")
    @Parameter(name = "goodsId", description = "商品ID", required = true)
    @Parameter(name = "authFlag", description = "审核结果", required = true)
    @PutMapping("/auth")
    public ResultMessage<Object> auth(@RequestParam List<String> goodsIds, @RequestParam String authFlag) {
        //校验商品是否存在
        if (goodsService.auditGoods(goodsIds, GoodsAuthEnum.valueOf(authFlag))) {
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.GOODS_AUTH_ERROR);
    }


    @PreventDuplicateSubmissions
    @Operation(description = "管理员上架商品")
    @Parameter(name = "goodsId", description = "商品ID", required = true)
    @PutMapping("/up")
    public ResultMessage<Object> unpGoods(@RequestParam List<String> goodsId) {
        if (Boolean.TRUE.equals(goodsService.updateGoodsMarketAble(goodsId, GoodsStatusEnum.UPPER, ""))) {
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.GOODS_UPPER_ERROR);
    }

    @PreventDuplicateSubmissions
    @Operation(description = "管理员下架商品")
    @Parameter(name = "goodsId", description = "商品ID", required = true)
    @Parameter(name = "reason", description = "下架理由", required = true)

    @DemoSite
    @PutMapping("/under")
    public ResultMessage<Object> underGoods(@RequestParam List<String> goodsId, @NotEmpty(message = "下架原因不能为空") @RequestParam String reason) {

        if (Boolean.TRUE.equals(goodsService.managerUpdateGoodsMarketAble(goodsId, GoodsStatusEnum.DOWN, reason))) {
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.GOODS_UNDER_ERROR);
    }


    @Operation(description = "通过id获取商品详情")
    @Parameter(name = "id", description = "商品ID", required = true)
    @GetMapping("/get/{id}")
    public ResultMessage<GoodsVO> get(@PathVariable String id) {
        GoodsVO goods = goodsService.getGoodsVO(id);
        return ResultUtil.data(goods);
    }

    @PreventDuplicateSubmissions
    @Operation(description = "设置商品虚拟销量")
    @Parameter(name = "skuId", description = "商品规格ID", required = true)
    @Parameter(name = "virtualSales", description = "虚拟销量", required = true)
    @PutMapping("/virtualSales/{skuId}")
    public ResultMessage<Object> updateVirtualSales(@PathVariable String skuId,
                                                    @NotNull(message = "虚拟销量不能为空")
                                                    @Min(value = 0, message = "虚拟销量不能小于0")
                                                    @Max(value = 99999999, message = "虚拟销量不能超过99999999")
                                                    @RequestParam Integer virtualSales) {
        goodsSkuService.updateGoodsSkuVirtualSales(skuId, virtualSales);
        return ResultUtil.success();
    }

    @PreventDuplicateSubmissions
    @Operation(description = "批量设置商品虚拟销量")
    @PutMapping("/virtualSales")
    public ResultMessage<Object> batchUpdateVirtualSales(@Valid @RequestBody GoodsVirtualSalesDTO goodsVirtualSalesDTO) {
        goodsSkuService.batchUpdateGoodsSkuVirtualSales(goodsVirtualSalesDTO.getSkuIds(), goodsVirtualSalesDTO.getVirtualSales());
        return ResultUtil.success();
    }

}
