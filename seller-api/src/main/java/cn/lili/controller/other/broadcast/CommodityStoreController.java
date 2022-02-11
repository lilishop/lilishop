package cn.lili.controller.other.broadcast;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Commodity;
import cn.lili.modules.goods.entity.vos.CommodityVO;
import cn.lili.modules.goods.service.CommodityService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 店铺端,直播商品接口
 *
 * @author Bulbasaur
 * @since 2021/5/17 2:05 下午
 */
@RestController
@Api(tags = "店铺端,直播商品接口")
@RequestMapping("/store/broadcast/commodity")
public class CommodityStoreController {

    @Autowired
    private CommodityService commodityService;

    @ApiOperation(value = "获取店铺直播商品列表")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "name", value = "商品名称", dataType = "String", paramType = "query"),
            @ApiImplicitParam(name = "auditStatus", value = "直播商品状态", dataType = "String", paramType = "query")
    })
    @GetMapping
    public ResultMessage<IPage<CommodityVO>> page(String auditStatus, String name, PageVO pageVO) {
        return ResultUtil.data(commodityService.commodityList(pageVO, name, auditStatus));
    }

    @ApiOperation(value = "添加店铺直播商品")
    @ApiImplicitParam(name = "commodityList", value = "直播商品列表", paramType = "body", allowMultiple = true, dataType = "Commodity")
    @PostMapping
    public ResultMessage<Object> addCommodity(@RequestBody List<Commodity> commodityList) {
        if (commodityService.addCommodity(commodityList)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @ApiOperation(value = "删除店铺直播商品")
    @ApiImplicitParam(name = "goodsId", value = "直播商品ID", dataType = "String", paramType = "path")
    @DeleteMapping("/{goodsId}")
    public ResultMessage<Object> delete(@PathVariable String goodsId) {
        if (commodityService.deleteCommodity(goodsId)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }
}
