package cn.lili.controller.manager.other.broadcast;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.vos.StudioVO;
import cn.lili.modules.goods.service.StudioService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.constraints.NotNull;

/**
 * 管理端,直播间接口
 *
 * @author Bulbasaur
 * @since 2021/5/28 11:56 上午
 */
@RestController
@Tag(name = "管理端,直播间接口")
@RequestMapping("/manager/broadcast/studio")
public class StudioManagerController {

    @Autowired
    private StudioService studioService;

    @Operation(description = "获取店铺直播间列表")
    @Parameter(name = "status", description = "直播间状态", required = false)
    @Parameter(name = "pageVO", description = "分页参数", required = false)
    @GetMapping
    public ResultMessage<IPage<StudioVO>> page(PageVO pageVO, String status) {
        return ResultUtil.data(studioService.studioList(pageVO, null, status));
    }

    @Operation(description = "获取店铺直播间详情")
    @Parameter(name = "studioId", description = "直播间ID", required = true)
    @GetMapping("/{studioId}")
    public ResultMessage<StudioVO> studioInfo(@PathVariable String studioId) {
        return ResultUtil.data(studioService.getStudioVO(studioId));
    }

    @Operation(description = "是否推荐直播间")
    @Parameter(name = "id", description = "直播间ID", required = true)
    @Parameter(name = "recommend", description = "是否推荐", required = true)
    @PutMapping("/recommend/{id}")
    public ResultMessage<Object> recommend(@PathVariable String id, @NotNull boolean recommend) {
        if (studioService.updateRecommend(id, recommend)) {
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.ERROR);
    }
}
