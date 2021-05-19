package cn.lili.controller.member;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.vos.CouponSearchParams;
import cn.lili.modules.promotion.entity.vos.CouponVO;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.MemberCouponService;
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

import javax.validation.constraints.NotNull;

/**
 * 买家端,买家优惠券接口
 *
 * @author paulG
 * @date 2020/11/17 3:35 下午
 */
@RestController
@Api(tags = "买家端,买家优惠券接口")
@RequestMapping("/buyer/promotion/coupon")
public class CouponBuyerController {

    /**
     * 优惠券
     */
    @Autowired
    private CouponService couponService;

    /**
     * 会员优惠券
     */
    @Autowired
    private MemberCouponService memberCouponService;

    @GetMapping
    @ApiOperation(value = "获取可领取优惠券列表")
    public ResultMessage<IPage<CouponVO>> getCouponList(CouponSearchParams queryParam, PageVO page) {
        IPage<CouponVO> canUseCoupons = couponService.getCanReceiveCoupons(queryParam, page);
        return ResultUtil.data(canUseCoupons);
    }

    @ApiOperation(value = "获取当前会员的优惠券列表")
    @GetMapping("/getCoupons")
    public ResultMessage<IPage<MemberCoupon>> getCoupons(CouponSearchParams param, PageVO pageVo) {
        param.setMemberId(UserContext.getCurrentUser().getId());
        return ResultUtil.data(memberCouponService.getMemberCoupons(param, pageVo));
    }

    @ApiOperation(value = "获取当前会员的对于当前商品可使用的优惠券列表")
    @GetMapping("/canUse")
    public ResultMessage<IPage<MemberCoupon>> getCouponsByCanUse(CouponSearchParams param, Double totalPrice, PageVO pageVo) {
        param.setMemberId(UserContext.getCurrentUser().getId());
        return ResultUtil.data(memberCouponService.getMemberCouponsByCanUse(param, totalPrice, pageVo));
    }

    @ApiOperation(value = "获取当前会员可使用的优惠券数量")
    @GetMapping("/getCouponsNum")
    public ResultMessage<Object> getMemberCouponsNum() {
        return ResultUtil.data(memberCouponService.getMemberCouponsNum());
    }

    @ApiOperation(value = "会员领取优惠券")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "couponId", value = "优惠券ID", required = true, dataType = "Long", paramType = "path")
    })
    @GetMapping("/receive/{couponId}")
    public ResultMessage<Object> receiveCoupon(@NotNull(message = "优惠券ID不能为空") @PathVariable("couponId") String couponId) {
        memberCouponService.checkCouponLimit(couponId, UserContext.getCurrentUser().getId());
        memberCouponService.receiveCoupon(couponId, UserContext.getCurrentUser().getId(), UserContext.getCurrentUser().getNickName());
        return ResultUtil.success();
    }

    @ApiOperation(value = "通过id获取")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "优惠券ID", required = true, dataType = "Long", paramType = "path")
    })
    @GetMapping(value = "/get/{id}")
    public ResultMessage<MemberCoupon> get(@NotNull(message = "优惠券ID不能为空") @PathVariable("id") String id) {
        MemberCoupon memberCoupon = memberCouponService.getById(id);
        return ResultUtil.data(memberCoupon);
    }


}
