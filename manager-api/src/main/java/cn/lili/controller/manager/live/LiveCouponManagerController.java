package cn.lili.controller.manager.live;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.live.entity.dos.LiveCoupon;
import cn.lili.modules.live.service.LiveCouponService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Slf4j
@Validated
@RestController
@Tag(name = "直播优惠券接口")
@RequestMapping("/manager/live/coupon")
@RequiredArgsConstructor
public class LiveCouponManagerController {

    private final LiveCouponService liveCouponService;

    @Operation(summary = "批量保存直播优惠券")
    @PostMapping("/batch")
    public ResultMessage<Object> saveBatch(@RequestBody List<LiveCoupon> liveCouponArray) {
        liveCouponService.saveBatchLiveCoupon(liveCouponArray);
        return ResultUtil.success();
    }

    @Operation(summary = "根据直播间ID获取直播优惠券列表")
    @GetMapping("/list/{liveId}")
    public ResultMessage<List<LiveCoupon>> getByLiveId(
            @NotNull(message = "直播间ID不能为空") @PathVariable String liveId) {
        return ResultUtil.data(liveCouponService.liveCouponList(liveId));
    }

    @Operation(summary = "根据优惠券ID获取直播优惠券")
    @GetMapping("/{couponId}")
    public ResultMessage<LiveCoupon> getByCouponId(
            @NotNull(message = "优惠券ID不能为空") @PathVariable String couponId) {
        return ResultUtil.data(liveCouponService.getById(couponId));
    }

    @Operation(summary = "设置推荐优惠券")
    @PutMapping("/recommend/{id}")
    public ResultMessage<Object> setRecommend(
            @NotNull(message = "优惠券ID不能为空") @PathVariable String id) {
        if (liveCouponService.setRecommend(id)) {
            return ResultUtil.success();
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @Operation(summary = "取消推荐优惠券")
    @DeleteMapping("/recommend/{id}")
    public ResultMessage<Object> cancelRecommend(
            @NotNull(message = "优惠券ID不能为空") @PathVariable String id) {
        if (liveCouponService.cancelRecommend(id)) {
            return ResultUtil.success();
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @Operation(summary = "批量删除直播优惠券")
    @DeleteMapping("/batch")
    public ResultMessage<Object> removeBatch(@RequestParam List<String> ids) {
        if (liveCouponService.removeLiveCoupon(ids)) {
            return ResultUtil.success();
        }
        return ResultUtil.error(ResultCode.ERROR);
    }
}
