package cn.lili.controller.seller.goods;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.dto.GoodsMarketScheduleDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsSkuStockDTO;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.vos.GoodsNumVO;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.statistics.aop.PageViewPoint;
import cn.lili.modules.statistics.aop.enums.PageViewEnum;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * 店铺端,商品接口
 *
 * @author pikachu
 * @since 2020-02-23 15:18:56
 */
@RestController
@Slf4j
@Tag(name = "店铺端,商品接口")
@RequestMapping("/store/goods/goods")
public class GoodsStoreController {

    /**
     * 商品
     */
    @Autowired
    private GoodsService goodsService;
    /**
     * 商品sku
     */
    @Autowired
    private GoodsSkuService goodsSkuService;


    @Operation(summary = "分页获取商品列表")
    @GetMapping("/list")
    public ResultMessage<IPage<Goods>> getByPage(GoodsSearchParams goodsSearchParams) {
        //获取当前登录商家账号
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        goodsSearchParams.setStoreId(storeId);
        return ResultUtil.data(goodsService.queryByParams(goodsSearchParams));
    }

    @Operation(summary = "获取商品数量")
    @GetMapping("/goodsNumber")
    public ResultMessage<GoodsNumVO> getGoodsNumVO(GoodsSearchParams goodsSearchParams) {
        //获取当前登录商家账号
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        goodsSearchParams.setStoreId(storeId);
        return ResultUtil.data(goodsService.getGoodsNumVO(goodsSearchParams));
    }

    @Operation(summary = "分页获取商品Sku列表")
    @GetMapping("/sku/list")
    public ResultMessage<IPage<GoodsSku>> getSkuByPage(GoodsSearchParams goodsSearchParams) {
        //获取当前登录商家账号
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        goodsSearchParams.setStoreId(storeId);
        return ResultUtil.data(goodsSkuService.getGoodsSkuByPage(goodsSearchParams));
    }

    @Operation(summary = "分页获取库存告警商品列表")
    @GetMapping("/list/stock")
    public ResultMessage<IPage<GoodsSku>> getWarningStockByPage(GoodsSearchParams goodsSearchParams) {
        //获取当前登录商家账号
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        goodsSearchParams.setStoreId(storeId);
        goodsSearchParams.setAlertQuantity(true);
        goodsSearchParams.setMarketEnable(GoodsStatusEnum.UPPER.name());
        IPage<GoodsSku> goodsSkuPage = goodsSkuService.getGoodsSkuByPage(goodsSearchParams);
        return ResultUtil.data(goodsSkuPage);
    }

    @Operation(summary = "批量修改商品预警库存")
    @PutMapping(value = "/batch/update/alert/stocks", consumes = "application/json")
    public ResultMessage<Object> batchUpdateAlertQuantity(@RequestBody List<GoodsSkuStockDTO> updateStockList) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        // 获取商品skuId集合
        List<String> goodsSkuIds = updateStockList.stream().map(GoodsSkuStockDTO::getSkuId).collect(Collectors.toList());
        // 根据skuId集合查询商品信息
        List<GoodsSku> goodsSkuList = goodsSkuService.listByIdsAndStoreId(goodsSkuIds, storeId);
        // 过滤不符合当前店铺的商品
        List<String> filterGoodsSkuIds = goodsSkuList.stream().map(GoodsSku::getId).collect(Collectors.toList());
        List<GoodsSkuStockDTO> collect = updateStockList.stream().filter(i -> filterGoodsSkuIds.contains(i.getSkuId())).collect(Collectors.toList());
        goodsSkuService.batchUpdateAlertQuantity(collect);
        return ResultUtil.success();
    }

    @Operation(summary = "修改商品预警库存")
    @PutMapping(value = "/update/alert/stocks", consumes = "application/json")
    public ResultMessage<Object> updateAlertQuantity(@RequestBody GoodsSkuStockDTO goodsSkuStockDTO) {
        goodsSkuService.updateAlertQuantity(goodsSkuStockDTO);
        return ResultUtil.success();
    }


    @Operation(summary = "通过id获取")
    @GetMapping("/get/{id}")
    public ResultMessage<GoodsVO> get(@PathVariable String id) {
        GoodsVO goods = OperationalJudgment.judgment(goodsService.getGoodsVO(id));
        return ResultUtil.data(goods);
    }

    @Operation(summary = "新增商品")
    @PostMapping(value = "/create", consumes = "application/json", produces = "application/json")
    public ResultMessage<GoodsOperationDTO> save(@Valid @RequestBody GoodsOperationDTO goodsOperationDTO) {
        goodsService.addGoods(goodsOperationDTO);
        return ResultUtil.success();
    }

    @Operation(summary = "修改商品")
    @PutMapping(value = "/update/{goodsId}", consumes = "application/json", produces = "application/json")
    public ResultMessage<GoodsOperationDTO> update(@Valid @RequestBody GoodsOperationDTO goodsOperationDTO, @PathVariable String goodsId) {
        goodsService.editGoods(goodsOperationDTO, goodsId);
        return ResultUtil.success();
    }

    @DemoSite
    @Operation(summary = "下架商品", description = "下架商品时使用")
    @Parameter(name = "goodsId", description = "商品ID", required = true)
    @PutMapping("/under")
    public ResultMessage<Object> underGoods(@RequestParam List<String> goodsId) {

        goodsService.updateGoodsMarketAble(goodsId, GoodsStatusEnum.DOWN, "商家下架");
        return ResultUtil.success();
    }

    @Operation(summary = "上架商品", description = "上架商品时使用")
    @PutMapping("/up")
    @Parameter(name = "goodsId", description = "商品ID", required = true)
    public ResultMessage<Object> unpGoods(@RequestParam List<String> goodsId) {
        goodsService.updateGoodsMarketAble(goodsId, GoodsStatusEnum.UPPER, "");
        return ResultUtil.success();
    }

    @DemoSite
    @Operation(summary = "删除商品")
    @PutMapping("/delete")
    @Parameter(name = "goodsId", description = "商品ID", required = true)
    public ResultMessage<Object> deleteGoods(@RequestParam List<String> goodsId) {
        goodsService.deleteGoods(goodsId);
        return ResultUtil.success();
    }

    @Operation(summary = "设置商品运费模板")
    @PutMapping("/freight")
    @Parameter(name = "goodsId", description = "商品ID", required = true)
    @Parameter(name = "templateId", description = "运费模板ID", required = true)
    public ResultMessage<Object> freight(@RequestParam List<String> goodsId, @RequestParam String templateId) {
        goodsService.freight(goodsId, templateId);
        return ResultUtil.success();
    }

    @Operation(summary = "根据goodsId分页获取商品规格列表")
    @GetMapping("/sku/{goodsId}/list")
    public ResultMessage<List<GoodsSkuVO>> getSkuByList(@PathVariable String goodsId) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(goodsSkuService.getGoodsSkuVOList(goodsSkuService.listByGoodsIdAndStoreId(goodsId, storeId)));
    }

    @Operation(summary = "修改商品库存")
    @PutMapping(value = "/update/stocks", consumes = "application/json")
    public ResultMessage<Object> updateStocks(@RequestBody List<GoodsSkuStockDTO> updateStockList) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        // 获取商品skuId集合
        List<String> goodsSkuIds = updateStockList.stream().map(GoodsSkuStockDTO::getSkuId).collect(Collectors.toList());
        // 根据skuId集合查询商品信息
        List<GoodsSku> goodsSkuList = goodsSkuService.listByIdsAndStoreId(goodsSkuIds, storeId);
        // 过滤不符合当前店铺的商品
        List<String> filterGoodsSkuIds = goodsSkuList.stream().map(GoodsSku::getId).collect(Collectors.toList());
        List<GoodsSkuStockDTO> collect = updateStockList.stream().filter(i -> filterGoodsSkuIds.contains(i.getSkuId())).collect(Collectors.toList());
        goodsSkuService.updateStocks(collect);
        return ResultUtil.success();
    }

    @Operation(summary = "定时上下架商品", description = "触发时间为到秒的Date类型，建议格式：yyyy-MM-dd HH:mm:ss")
    @PostMapping(value = "/schedule/market", consumes = "application/json")
    public ResultMessage<Object> scheduleMarket(@RequestBody @Valid GoodsMarketScheduleDTO dto) {
        GoodsStatusEnum status = GoodsStatusEnum.valueOf(dto.getStatus());
        goodsService.scheduleGoodsMarket(dto.getGoodsIds(), status, dto.getTriggerTime(), dto.getReason());
        return ResultUtil.success();
    }

    @Operation(summary = "通过id获取商品信息")
    @Parameter(name = "goodsId", description = "商品ID", required = true)
    @Parameter(name = "skuId", description = "skuId", required = true)
    @GetMapping("/sku/{goodsId}/{skuId}")
    @PageViewPoint(type = PageViewEnum.SKU, id = "#id")
    public ResultMessage<Map<String, Object>> getSku(@NotNull(message = "商品ID不能为空") @PathVariable("goodsId") String goodsId,
                                                     @NotNull(message = "SKU ID不能为空") @PathVariable("skuId") String skuId) {
        try {
            // 读取选中的列表
            Map<String, Object> map = goodsSkuService.getGoodsSkuDetail(goodsId, skuId);
            return ResultUtil.data(map);
        } catch (ServiceException se) {
            log.info(se.getMsg(), se);
            throw se;
        } catch (Exception e) {
            log.error(ResultCode.GOODS_ERROR.message(), e);
            return ResultUtil.error(ResultCode.GOODS_ERROR);
        }

    }


    @Operation(summary = "分页获取商品Sku列表")
    @GetMapping("/queryExportStock")
    public void queryExportStock(GoodsSearchParams goodsSearchParams) {
        //获取当前登录商家账号
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        goodsSearchParams.setStoreId(storeId);

        HttpServletResponse response = ThreadContextHolder.getHttpResponse();
        goodsSkuService.queryExportStock(response, goodsSearchParams);
    }

    @Operation(summary = "上传商品库存列表")
    @PostMapping(value = "/importStockExcel", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResultMessage<Object> importStockExcel(@RequestPart("files") MultipartFile files) {
        //获取当前登录商家账号
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        goodsSkuService.importStock(storeId, files);
        return ResultUtil.success(ResultCode.SUCCESS);
    }


}
