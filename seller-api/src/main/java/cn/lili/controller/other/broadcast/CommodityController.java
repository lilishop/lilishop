package cn.lili.controller.other.broadcast;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.broadcast.entity.dos.Commodity;
import cn.lili.modules.broadcast.service.CommodityService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 店铺端,直播商品接口
 *
 * @author Bulbasaur
 * @date: 2021/5/17 2:05 下午
 */
@RestController
@Api(tags = "店铺端,直播商品接口")
@RequestMapping("/store/broadcast/commodity")
public class CommodityController {

    @Autowired
    private CommodityService commodityService;

    @ApiOperation(value = "获取店铺直播商品列表")
    @GetMapping
    public ResultMessage<IPage<Commodity>> page(PageVO pageVO) {
        return ResultUtil.data(commodityService.page(PageUtil.initPage(pageVO)));
    }

    @ApiOperation(value = "添加店铺直播商品")
    @PostMapping
    public ResultMessage<Object> addCommodity(@Validated Commodity commodity) {
        if (commodityService.addCommodity(commodity)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @ApiOperation(value = "删除店铺直播商品")
    @DeleteMapping
    public ResultMessage<Object> delete(String goodsId) {
        if (commodityService.deleteCommodity(goodsId)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }
}
