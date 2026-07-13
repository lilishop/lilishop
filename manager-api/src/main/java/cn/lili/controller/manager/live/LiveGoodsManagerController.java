package cn.lili.controller.manager.live;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.live.entity.dos.LiveGoods;
import cn.lili.modules.live.service.LiveGoodsService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 直播商品接口
 * 
 * @author chc
 * @since 2022/6/21 14:46
 */
@Slf4j
@Validated
@RestController
@Tag(name = "直播商品接口")
@RequestMapping("/manager/live/goods")
@RequiredArgsConstructor
public class LiveGoodsManagerController {

    private final LiveGoodsService liveGoodsService;

    @Operation(summary = "批量保存直播商品")
    @PostMapping("/batch")
    public ResultMessage<Object> saveBatch(@RequestBody List<LiveGoods> liveGoodsArray) {
        liveGoodsService.saveBatchLiveGoodsArray(liveGoodsArray);
        return ResultUtil.success();
    }

    @Operation(summary = "根据直播间ID获取直播商品列表")
    @GetMapping("/list/{liveId}")
    public ResultMessage<List<LiveGoods>> getByLiveId(
            @NotNull(message = "直播间ID不能为空") @PathVariable String liveId) {
        return ResultUtil.data(liveGoodsService.queryByLiveId(liveId));
    }

    @Operation(summary = "从缓存获取直播商品列表")
    @GetMapping("/cache/{liveId}")
    public ResultMessage<List<LiveGoods>> getByCache(
            @NotNull(message = "直播间ID不能为空") @PathVariable String liveId) {
        return ResultUtil.data(liveGoodsService.getByCache(liveId));
    }

    @Operation(summary = "批量更新显示状态")
    @PutMapping("/show")
    public ResultMessage<Object> batchUpdateShowStatus(
            @RequestParam List<String> ids,
            @RequestParam Boolean show) {
        if (liveGoodsService.batchUpdateShowStatus(ids, show)) {
            return ResultUtil.success();
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @Operation(summary = "设置推荐商品")
    @PutMapping("/recommend/{id}")
    public ResultMessage<Object> setRecommend(
            @NotNull(message = "商品ID不能为空") @PathVariable String id) {
        if (liveGoodsService.setRecommend(id)) {
            return ResultUtil.success();
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @Operation(summary = "取消推荐商品")
    @DeleteMapping("/recommend/{id}")
    public ResultMessage<Object> cancelRecommend(
            @NotNull(message = "商品ID不能为空") @PathVariable String id) {
        if (liveGoodsService.cancelRecommend(id)) {
            return ResultUtil.success();
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @Operation(summary = "批量删除直播商品")
    @DeleteMapping("/batch")
    public ResultMessage<Object> removeBatch(@RequestBody List<String> ids) {
        if (liveGoodsService.removeLiveGoods(ids)) {
            return ResultUtil.success();
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @Operation(summary = "设置售罄状态")
    @PutMapping("/sold-out/{id}")
    public ResultMessage<Object> setSoldOut(
            @NotNull(message = "商品ID不能为空") @PathVariable String id,
            @RequestParam Boolean soldOutFlag) {
        if (liveGoodsService.setSoldOut(id, soldOutFlag)) {
            return ResultUtil.success();
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @Operation(summary = "增加商品热度")
    @PutMapping("/popularity/{id}")
    public ResultMessage<Object> setPopularity(
            @NotNull(message = "商品ID不能为空") @PathVariable String id,
            @NotNull(message = "直播间ID不能为空") @RequestParam String liveId) {
        liveGoodsService.addPopularity(id, liveId);
        return ResultUtil.success();
    }

    @Operation(summary = "设置商品热度")
    @PutMapping("/set/popularity/{id}")
    public ResultMessage<Object> getPopularity(
            @NotNull(message = "商品ID不能为空") @PathVariable String id,
            @NotNull(message = "直播间ID不能为空") @RequestParam String liveId,  @NotNull(message = "热度不能为空") @RequestParam Integer popularity) {
        liveGoodsService.setPopularity(id, liveId, popularity);
        return ResultUtil.success();
    }

    @Operation(summary = "获取商品热度")
    @GetMapping("/popularity/{id}")
    public ResultMessage<Long> getPopularity(
            @NotNull(message = "商品ID不能为空") @PathVariable String id,
            @NotNull(message = "直播间ID不能为空") @RequestParam String liveId) {
        return ResultUtil.data(liveGoodsService.getPopularity(id, liveId));
    }

    @Operation(summary = "根据ID获取直播商品")
    @GetMapping("/ids")
    public ResultMessage<List<LiveGoods>> getByIds(@RequestParam List<String> ids) {
        return ResultUtil.data(liveGoodsService.getLiveGoodsById(ids));
    }

    @Operation(summary = "通过id获取直播商品")
    @GetMapping("/{id}")
    public ResultMessage<LiveGoods> getById(@NotNull(message = "商品ID不能为空") @PathVariable String id) {
        return ResultUtil.data(liveGoodsService.getById(id));
    }
}
