package cn.lili.controller.other.broadcast;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.broadcast.entity.dos.Studio;
import cn.lili.modules.broadcast.service.StudioService;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,直播间接口
 *
 * @author Bulbasaur
 * @date: 2021/5/28 11:56 上午
 */
@RestController
@Api(tags = "店铺端,直播商品接口")
@RequestMapping("/manager/broadcast/studio")
public class StudioManagerController {

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
    public ResultMessage<Studio> studioInfo(@PathVariable String studioId) {
        return ResultUtil.data(studioService.getById(studioId));
    }

    @ApiOperation(value = "是否推荐直播间")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "Id", required = true, dataType = "String", paramType = "path"),
            @ApiImplicitParam(name = "recommend", value = "是否推荐", required = true, dataType = "boolean", paramType = "path")
    })
    @GetMapping("/id/{studioId}")
    public ResultMessage<Object> recommend(@PathVariable String id, @PathVariable boolean recommend) {
        if (studioService.update(new UpdateWrapper<Studio>()
                .eq("id", id)
                .set("recommend", recommend))) {
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.ERROR);
    }
}
