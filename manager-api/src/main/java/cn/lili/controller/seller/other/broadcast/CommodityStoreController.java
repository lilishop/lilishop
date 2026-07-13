package cn.lili.controller.seller.other.broadcast;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Commodity;
import cn.lili.modules.goods.entity.vos.CommodityVO;
import cn.lili.modules.goods.service.CommodityService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "店铺端,直播商品接口")
@RequestMapping("/store/broadcast/commodity")
public class CommodityStoreController {

    @Autowired
    private CommodityService commodityService;

    @Operation(description = "获取店铺直播商品列表")
    @Parameter(name = "name", description = "商品名称", required = false)
    @Parameter(name = "auditStatus", description = "直播商品状态", required = false)
    @GetMapping
    public ResultMessage<IPage<CommodityVO>> page(String auditStatus, String name, PageVO pageVO) {
        return ResultUtil.data(commodityService.commodityList(pageVO, name, auditStatus));
    }

    @Operation(description = "添加店铺直播商品")
    @Parameter(name = "commodityList", description = "直播商品列表", required = true)
    @PostMapping
    public ResultMessage<Object> addCommodity(@RequestBody List<Commodity> commodityList) {
        if (commodityService.addCommodity(commodityList)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @Operation(description = "删除店铺直播商品")
    @Parameter(name = "goodsId", description = "直播商品ID", required = true)
    @DeleteMapping("/{goodsId}")
    public ResultMessage<Object> delete(@PathVariable String goodsId) {
        if (commodityService.deleteCommodity(goodsId)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }
}
