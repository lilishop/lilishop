package cn.lili.controller.manager.other.broadcast;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.vos.CommodityVO;
import cn.lili.modules.goods.service.CommodityService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "店铺端,直播商品接口")
@RequestMapping("/manager/broadcast/commodity")
public class CommodityManagerController {

    @Autowired
    private CommodityService commodityService;

    @Operation(description = "获取店铺直播商品列表")
    @Parameter(name = "auditStatus", description = "直播商品状态", required = false)
    @Parameter(name = "name", description = "商品名称", required = false)
    @Parameter(name = "pageVO", description = "分页参数", required = false)
    @GetMapping
    public ResultMessage<IPage<CommodityVO>> page(String auditStatus, String name, PageVO pageVO) {
        return ResultUtil.data(commodityService.commodityList(pageVO, name, auditStatus));
    }
}
