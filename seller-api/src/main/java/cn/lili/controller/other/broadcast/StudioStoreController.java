package cn.lili.controller.other.broadcast;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.broadcast.entity.dos.Studio;
import cn.lili.modules.broadcast.entity.vos.StudioVO;
import cn.lili.modules.broadcast.service.StudioService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 店铺端,直播间接口
 *
 * @author Bulbasaur
 * @date: 2021/5/17 2:05 下午
 */
@RestController
@Api(tags = "店铺端,直播间接口")
@RequestMapping("/store/broadcast/studio")
public class StudioStoreController {

    @Autowired
    private StudioService studioService;

    @ApiOperation(value = "获取店铺直播间列表")
    @ApiImplicitParam(name = "status", value = "直播间状态", paramType = "query", dataType = "String")
    @GetMapping
    public ResultMessage<IPage<Studio>> page(PageVO pageVO, String status) {
        return ResultUtil.data(studioService.studioList(pageVO, null, status));
    }

    @ApiOperation(value = "获取店铺直播间详情")
    @ApiImplicitParam(name = "studioId", value = "直播间ID", required = true, dataType = "String", paramType = "path")
    @GetMapping("/studioInfo/{studioId}")
    public ResultMessage<StudioVO> studioInfo(@PathVariable String studioId) {
        return ResultUtil.data(studioService.getStudioVO(studioId));
    }

    @ApiOperation(value = "添加直播间")
    @PostMapping
    public ResultMessage<Object> add(@Validated Studio studio) {
        if (studioService.create(studio)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @ApiOperation(value = "修改直播间")
    @PutMapping("/edit")
    public ResultMessage<Object> edit(Studio studio) {
        if (studioService.edit(studio)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @ApiOperation(value = "店铺直播间添加商品")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "roomId", value = "房间ID", required = true, dataType = "Integer", paramType = "path"),
            @ApiImplicitParam(name = "liveGoodsId", value = "直播商品ID", required = true, dataType = "Integer", paramType = "path")
    })
    @PutMapping(value = "/push/{roomId}/{liveGoodsId}")
    public ResultMessage<Studio> push(@PathVariable Integer roomId, @PathVariable Integer liveGoodsId) {
        if (studioService.push(roomId, liveGoodsId)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @ApiOperation(value = "店铺直播间删除商品")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "roomId", value = "房间ID", required = true, dataType = "Integer", paramType = "path"),
            @ApiImplicitParam(name = "liveGoodsId", value = "直播商品ID", required = true, dataType = "Integer", paramType = "path")
    })
    @DeleteMapping(value = "/deleteInRoom/{roomId}/{liveGoodsId}")
    public ResultMessage<Studio> deleteInRoom(@PathVariable Integer roomId, @PathVariable Integer liveGoodsId) {
        if (studioService.goodsDeleteInRoom(roomId, liveGoodsId)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }
}
