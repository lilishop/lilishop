package cn.lili.controller.other.broadcast;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Studio;
import cn.lili.modules.goods.entity.vos.StudioVO;
import cn.lili.modules.goods.service.StudioService;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;

/**
 * 管理端,直播间接口
 *
 * @author Bulbasaur
 * @since 2021/5/28 11:56 上午
 */
@RestController
@Api(tags = "管理端,直播间接口")
@RequestMapping("/manager/broadcast/studio")
public class StudioManagerController {

    @Autowired
    private StudioService studioService;

    @ApiOperation(value = "获取店铺直播间列表")
    @ApiImplicitParam(name = "status", value = "直播间状态", paramType = "query")
    @GetMapping
    public ResultMessage<IPage<StudioVO>> page(PageVO pageVO, String status) {
        return ResultUtil.data(studioService.studioList(pageVO, null, status));
    }

    @ApiOperation(value = "获取店铺直播间详情")
    @ApiImplicitParam(name = "studioId", value = "直播间ID", required = true, paramType = "path")
    @GetMapping("/{studioId}")
    public ResultMessage<StudioVO> studioInfo(@PathVariable String studioId) {
        return ResultUtil.data(studioService.getStudioVO(studioId));
    }

    @ApiOperation(value = "是否推荐直播间")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "Id", required = true, paramType = "path"),
            @ApiImplicitParam(name = "recommend", value = "是否推荐", required = true, paramType = "query")
    })
    @PutMapping("/recommend/{id}")
    public ResultMessage<Object> recommend(@PathVariable String id, @NotNull boolean recommend) {
        if (studioService.update(new UpdateWrapper<Studio>()
                .eq("id", id)
                .set("recommend", recommend))) {
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.ERROR);
    }
}
