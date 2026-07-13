package cn.lili.controller.seller.other.broadcast;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dos.Studio;
import cn.lili.modules.goods.entity.vos.StudioVO;
import cn.lili.modules.goods.service.StudioService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Objects;

/**
 * 店铺端,直播间接口
 *
 * @author Bulbasaur
 * @since 2021/5/17 2:05 下午
 */
@RestController
@Tag(name = "店铺端,直播间接口")
@RequestMapping("/store/broadcast/studio")
public class StudioStoreController {

    @Autowired
    private StudioService studioService;

    @Operation(description = "获取店铺直播间列表")
    @Parameter(name = "status", description = "直播间状态", required = false)
    @GetMapping
    public ResultMessage<IPage<StudioVO>> page(PageVO pageVO, String status) {
        return ResultUtil.data(studioService.studioList(pageVO, null, status));
    }

    @Operation(description = "获取店铺直播间详情")
    @Parameter(name = "studioId", description = "直播间ID", required = true)
    @GetMapping("/studioInfo/{studioId}")
    public ResultMessage<StudioVO> studioInfo(@PathVariable String studioId) {
        return ResultUtil.data(OperationalJudgment.judgment(studioService.getStudioVO(studioId)));
    }

    @Operation(description = "添加直播间")
    @PostMapping
    public ResultMessage<Object> add(@Validated Studio studio) {
        if (Boolean.TRUE.equals(studioService.create(studio))) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @Operation(description = "修改直播间")
    @Parameter(name = "studio", description = "直播间信息", required = true)    
    @PutMapping("/edit")
    public ResultMessage<Object> edit(Studio studio) {
        OperationalJudgment.judgment(studioService.getById(studio.getId()));
        if (Boolean.TRUE.equals(studioService.edit(studio))) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @Operation(description = "店铺直播间添加商品")
    @Parameter(name = "roomId", description = "房间ID", required = true)
    @Parameter(name = "liveGoodsId", description = "直播商品ID", required = true)
    @Parameter(name = "goodsId", description = "商品ID", required = true)
    @PutMapping("/push/{roomId}/{liveGoodsId}")
    public ResultMessage<Studio> push(@PathVariable Integer roomId, @PathVariable Integer liveGoodsId, @RequestParam String goodsId) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        if (Boolean.TRUE.equals(studioService.push(roomId, liveGoodsId, storeId, goodsId))) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @Operation(description = "店铺直播间删除商品")
    @Parameter(name = "roomId", description = "房间ID", required = true)
    @Parameter(name = "liveGoodsId", description = "直播商品ID", required = true)

    @DeleteMapping("/deleteInRoom/{roomId}/{liveGoodsId}")
    public ResultMessage<Studio> deleteInRoom(@PathVariable Integer roomId, @PathVariable Integer liveGoodsId) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        if (Boolean.TRUE.equals(studioService.goodsDeleteInRoom(roomId, liveGoodsId, storeId))) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }
}
