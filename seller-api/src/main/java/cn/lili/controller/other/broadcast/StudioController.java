package cn.lili.controller.other.broadcast;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.base.entity.enums.ClientTypeEnum;
import cn.lili.modules.broadcast.entity.dos.Studio;
import cn.lili.modules.broadcast.service.StudioService;
import cn.lili.modules.message.util.WechatAccessTokenUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.apache.ibatis.annotations.Delete;
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
public class StudioController {

    @Autowired
    private StudioService studioService;
    @Autowired
    private WechatAccessTokenUtil wechatAccessTokenUtil;

    @ApiOperation(value = "获取店铺直播间列表")
    @GetMapping
    public ResultMessage<IPage<Studio>> page(PageVO pageVO) {
        return ResultUtil.data(studioService.page(PageUtil.initPage(pageVO)));
    }

    @ApiOperation(value = "获取店铺直播间详情")
    @ApiImplicitParam(name = "studioId", value = "直播间ID", required = true, dataType = "String", paramType = "path")
    @GetMapping("/studioInfo/{studioId}")
    public ResultMessage<Studio> studioInfo(@PathVariable String studioId) {
        return ResultUtil.data(studioService.getById(studioId));
    }

    @ApiOperation(value = "添加直播间")
    @PostMapping
    public ResultMessage<Object> add(@Validated Studio studio) {
        if (studioService.create(studio)) {
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
    @Delete(value = "/deleteInRoom/{roomId}/{liveGoodsId}")
    public ResultMessage<Studio> deleteInRoom(@PathVariable Integer roomId, @PathVariable Integer liveGoodsId) {
        if (studioService.goodsDeleteInRoom(roomId, liveGoodsId)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @ApiOperation(value = "获取素材，调用接口凭证")
    @PutMapping(value = "/getCgiAccessToken")
    public ResultMessage<Object> getCgiAccessToken() {
        return ResultUtil.data(wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP));
    }
}
