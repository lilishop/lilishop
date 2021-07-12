package cn.lili.controller.goods;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsSkuStockDTO;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.entity.vos.StockWarningVO;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.store.service.StoreDetailService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 店铺端,商品接口
 *
 * @author pikachu
 * @date 2020-02-23 15:18:56
 */
@RestController
@Api(tags = "店铺端,商品接口")
@RequestMapping("/store/goods")
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
    /**
     * 店铺详情
     */
    @Autowired
    private StoreDetailService storeDetailService;

    @ApiOperation(value = "分页获取商品列表")
    @GetMapping(value = "/list")
    public ResultMessage<IPage<Goods>> getByPage(GoodsSearchParams goodsSearchParams) {

        //获取当前登录商家账号
        AuthUser tokenUser = UserContext.getCurrentUser();
        goodsSearchParams.setStoreId(tokenUser.getStoreId());
        return ResultUtil.data(goodsService.queryByParams(goodsSearchParams));
    }

    @ApiOperation(value = "分页获取商品Sku列表")
    @GetMapping(value = "/sku/list")
    public ResultMessage<IPage<GoodsSku>> getSkuByPage(GoodsSearchParams goodsSearchParams) {

        //获取当前登录商家账号
        AuthUser tokenUser = UserContext.getCurrentUser();
        goodsSearchParams.setStoreId(tokenUser.getStoreId());
        return ResultUtil.data(goodsSkuService.getGoodsSkuByPage(goodsSearchParams));
    }

    @ApiOperation(value = "分页获取库存告警商品列表")
    @GetMapping(value = "/list/stock")
    public ResultMessage<StockWarningVO> getWarningStockByPage(GoodsSearchParams goodsSearchParams) {
        //获取当前登录商家账号
        AuthUser tokenUser = UserContext.getCurrentUser();
        Integer stockWarnNum = storeDetailService.getStoreDetail(tokenUser.getStoreId()).getStockWarning();
        goodsSearchParams.setStoreId(tokenUser.getStoreId());
        goodsSearchParams.setQuantity(storeDetailService.getStoreDetail(tokenUser.getStoreId()).getStockWarning());
        goodsSearchParams.setMarketEnable(GoodsStatusEnum.UPPER.name());
        IPage<GoodsSku> goodsSku = goodsSkuService.getGoodsSkuByPage(goodsSearchParams);
        StockWarningVO stockWarning = new StockWarningVO(stockWarnNum, goodsSku);
        return ResultUtil.data(stockWarning);
    }


    @ApiOperation(value = "通过id获取")
    @GetMapping(value = "/get/{id}")
    public ResultMessage<GoodsVO> get(@PathVariable String id) {
        AuthUser tokenUser = UserContext.getCurrentUser();
        GoodsVO goods = goodsService.getGoodsVO(id);
        if (tokenUser.getStoreId().equals(goods.getStoreId())) {
            return ResultUtil.data(goods);
        }
        throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
    }

    @ApiOperation(value = "新增商品")
    @PostMapping(value = "/create", consumes = "application/json", produces = "application/json")
    public ResultMessage<GoodsOperationDTO> save(@RequestBody GoodsOperationDTO goodsOperationDTO) {

        goodsService.addGoods(goodsOperationDTO);
        return ResultUtil.success();
    }

    @ApiOperation(value = "修改商品")
    @PutMapping(value = "/update/{goodsId}", consumes = "application/json", produces = "application/json")
    public ResultMessage<GoodsOperationDTO> update(@RequestBody GoodsOperationDTO goodsOperationDTO, @PathVariable String goodsId) {
        goodsService.editGoods(goodsOperationDTO, goodsId);
        return ResultUtil.success();
    }

    @ApiOperation(value = "下架商品", notes = "下架商品时使用")
    @ApiImplicitParam(name = "goodsId", value = "商品ID", required = true, paramType = "query", allowMultiple = true)
    @PutMapping(value = "/under")
    public ResultMessage<Object> underGoods(@RequestParam List<String> goodsId) {

        goodsService.updateGoodsMarketAble(goodsId, GoodsStatusEnum.DOWN, "商家下架");
        return ResultUtil.success();
    }

    @ApiOperation(value = "上架商品", notes = "上架商品时使用")
    @PutMapping(value = "/up")
    @ApiImplicitParam(name = "goodsId", value = "商品ID", required = true, paramType = "query", allowMultiple = true)
    public ResultMessage<Object> unpGoods(@RequestParam List<String> goodsId) {
        goodsService.updateGoodsMarketAble(goodsId, GoodsStatusEnum.UPPER, "");
        return ResultUtil.success();
    }

    @ApiOperation(value = "删除商品")
    @PutMapping(value = "/delete")
    @ApiImplicitParam(name = "goodsId", value = "商品ID", required = true, paramType = "query", allowMultiple = true)
    public ResultMessage<Object> deleteGoods(@RequestParam List<String> goodsId) {
        goodsService.deleteGoods(goodsId);
        return ResultUtil.success();
    }

    @ApiOperation(value = "设置商品运费模板")
    @PutMapping(value = "/freight")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "goodsId", value = "商品ID", required = true, paramType = "query", allowMultiple = true),
            @ApiImplicitParam(name = "templateId", value = "运费模板ID", required = true, paramType = "query")
    })
    public ResultMessage<Object> freight(@RequestParam List<String> goodsId, @RequestParam String templateId) {
        goodsService.freight(goodsId, templateId);
        return ResultUtil.success();
    }

    @ApiOperation(value = "根据goodsId分页获取商品规格列表")
    @GetMapping(value = "/sku/{goodsId}/list")
    public ResultMessage<List<GoodsSkuVO>> getSkuByList(@PathVariable String goodsId) {

        return ResultUtil.data(goodsSkuService.getGoodsListByGoodsId(goodsId));
    }

    @ApiOperation(value = "修改商品库存")
    @PutMapping(value = "/update/stocks", consumes = "application/json")
    public ResultMessage<Object> updateStocks(@RequestBody List<GoodsSkuStockDTO> updateStockList) {
        goodsSkuService.updateStocks(updateStockList);
        return ResultUtil.success();
    }

}
