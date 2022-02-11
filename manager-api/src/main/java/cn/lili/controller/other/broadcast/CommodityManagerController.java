package cn.lili.controller.other.broadcast;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.vos.CommodityVO;
import cn.lili.modules.goods.service.CommodityService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,直播间管理接口
 *
 * @author Bulbasaur
 * @since 2021/5/28 11:56 上午
 */
@RestController
@Api(tags = "店铺端,直播商品接口")
@RequestMapping("/manager/broadcast/commodity")
public class CommodityManagerController {

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
}
