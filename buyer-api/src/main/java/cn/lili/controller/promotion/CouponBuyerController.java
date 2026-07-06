package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dto.CouponActivityTrigger;
import cn.lili.modules.promotion.entity.dto.search.CouponSearchParams;
import cn.lili.modules.promotion.entity.dto.search.MemberCouponSearchParams;
import cn.lili.modules.promotion.entity.enums.CouponActivityTypeEnum;
import cn.lili.modules.promotion.entity.enums.CouponGetEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.entity.vos.CouponVO;
import cn.lili.modules.promotion.service.CouponActivityService;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.MemberCouponService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import jakarta.validation.constraints.NotNull;
import java.util.List;
import java.util.Objects;

/**
 * 买家端,买家优惠券接口
 *
 * @author paulG
 * @since 2020/11/17 3:35 下午
 */
@RestController
@Tag(name = "买家端,买家优惠券接口")
@RequestMapping("/buyer/promotion/coupon")
public class CouponBuyerController {

    /**
     * 优惠券
     */
    @Autowired
    private CouponService couponService;

    /**
     * 优惠券活动
     */
    @Autowired
    private CouponActivityService couponActivityService;

    /**
     * 客户优惠券
     */
    @Autowired
    private MemberCouponService memberCouponService;

    @GetMapping("/activity")
    @Operation(summary = "自动领取优惠券")
    public ResultMessage<List<MemberCoupon>> activity() {
        if (UserContext.getCurrentUser() == null) {
            return ResultUtil.success();
        }
        List<MemberCoupon> memberCouponList = couponActivityService.trigger(
                CouponActivityTrigger.builder()
                        .couponActivityTypeEnum(CouponActivityTypeEnum.AUTO_COUPON)
                        .nickName(UserContext.getCurrentUser().getNickName())
                        .userId(UserContext.getCurrentUser().getId())
                        .build());
        memberCouponList.addAll(couponActivityService.trigger(
                CouponActivityTrigger.builder()
                        .couponActivityTypeEnum(CouponActivityTypeEnum.SPECIFY)
                        .nickName(UserContext.getCurrentUser().getNickName())
                        .userId(UserContext.getCurrentUser().getId())
                        .build()));
        return ResultUtil.data(memberCouponList);
    }

    @GetMapping
    @Operation(summary = "获取可领取优惠券列表")
    public ResultMessage<IPage<CouponVO>> getCouponList(CouponSearchParams queryParam, PageVO page) {
        queryParam.setPromotionStatus(PromotionsStatusEnum.START.name());
        queryParam.setGetType(CouponGetEnum.FREE.name());
        IPage<CouponVO> canUseCoupons = couponService.pageVOFindAll(queryParam, page);
        return ResultUtil.data(canUseCoupons);
    }

    @Operation(summary = "获取当前客户的优惠券列表")
    @GetMapping("/getCoupons")
    public ResultMessage<IPage<MemberCoupon>> getCoupons(MemberCouponSearchParams param, PageVO pageVo) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        param.setMemberId(currentUser.getId());
        return ResultUtil.data(memberCouponService.getMemberCoupons(param, pageVo));
    }

    @Operation(summary = "获取当前客户的对于当前商品可使用的优惠券列表")
    @GetMapping("/canUse")
    public ResultMessage<IPage<MemberCoupon>> getCouponsByCanUse(MemberCouponSearchParams param, Double totalPrice, PageVO pageVo) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        param.setMemberId(currentUser.getId());
        return ResultUtil.data(memberCouponService.getMemberCouponsByCanUse(param, totalPrice, pageVo));
    }

    @Operation(summary = "获取当前客户可使用的优惠券数量")
    @GetMapping("/getCouponsNum")
    public ResultMessage<Object> getMemberCouponsNum() {
        return ResultUtil.data(memberCouponService.getMemberCouponsNum());
    }

    @Operation(summary = "客户领取优惠券")
    @Parameters({
            @Parameter(name = "couponId", description = "优惠券ID", required = true)
    })
    @GetMapping("/receive/{couponId}")
    public ResultMessage<Object> receiveCoupon(@NotNull(message = "优惠券ID不能为空") @PathVariable("couponId") String couponId) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        memberCouponService.receiveBuyerCoupon(couponId, currentUser.getId(), currentUser.getNickName());
        return ResultUtil.success();
    }

    @Operation(summary = "客户领取直播优惠券")
    @Parameters({
            @Parameter(name = "liveRoomId", description = "直播间ID", required = true),
            @Parameter(name = "couponId", description = "优惠券ID", required = true)
    })
    @GetMapping("/receive/live/{liveRoomId}/{couponId}")
    public ResultMessage<MemberCoupon> receiveLiveCoupon(
            @NotNull(message = "直播间ID不能为空") @PathVariable("liveRoomId") String liveRoomId,
            @NotNull(message = "优惠券ID不能为空") @PathVariable("couponId") String couponId) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        MemberCoupon memberCoupon = memberCouponService.receiveLiveBuyerCoupon(
                couponId, liveRoomId, currentUser.getId(), currentUser.getNickName());
        return ResultUtil.data(memberCoupon);
    }

    @Operation(summary = "通过id获取")
    @Parameters({
            @Parameter(name = "id", description = "优惠券ID", required = true)
    })
    @GetMapping("/get/{id}")
    public ResultMessage<MemberCoupon> get(@NotNull(message = "优惠券ID不能为空") @PathVariable("id") String id) {
        MemberCoupon memberCoupon = OperationalJudgment.judgment(memberCouponService.getById(id));
        return ResultUtil.data(memberCoupon);
    }


}
