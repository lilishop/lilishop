package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.vos.CouponSearchParams;
import cn.lili.modules.promotion.entity.vos.CouponVO;
import cn.lili.modules.promotion.service.CouponService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;
import java.util.List;

/**
 * 店铺端,优惠券接口
 *
 * @author paulG
 * @date 2020/8/28
 **/
@RestController
@Api(tags = "店铺端,优惠券接口")
@RequestMapping("/store/promotion/coupon")
public class CouponStoreController {

    @Autowired
    private CouponService couponService;

    @GetMapping
    @ApiOperation(value = "获取优惠券列表")
    public ResultMessage<IPage<CouponVO>> getCouponList(CouponSearchParams queryParam, PageVO page) {
        page.setNotConvert(true);
        AuthUser currentUser = UserContext.getCurrentUser();
        queryParam.setStoreId(currentUser.getStoreId());
        IPage<CouponVO> coupons = couponService.getCouponsByPageFromMongo(queryParam, page);
        return ResultUtil.data(coupons);
    }

    @ApiOperation(value = "获取优惠券详情")
    @GetMapping("/{couponId}")
    public ResultMessage<Coupon> getCouponList(@PathVariable String couponId) {
        AuthUser currentUser = UserContext.getCurrentUser();
        Coupon coupon = couponService.getCouponDetailFromMongo(couponId);
        if (coupon == null || !coupon.getStoreId().equals(currentUser.getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        return ResultUtil.data(coupon);
    }

    @ApiOperation(value = "添加优惠券")
    @PostMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<CouponVO> addCoupon(@RequestBody CouponVO couponVO) {
        AuthUser currentUser = UserContext.getCurrentUser();
        couponVO.setStoreId(currentUser.getStoreId());
        couponVO.setStoreName(currentUser.getStoreName());
        couponService.add(couponVO);
        return ResultUtil.data(couponVO);
    }

    @PutMapping(consumes = "application/json", produces = "application/json")
    @ApiOperation(value = "修改优惠券")
    public ResultMessage<Coupon> updateCoupon(@RequestBody CouponVO couponVO) {
        AuthUser currentUser = UserContext.getCurrentUser();
        couponVO.setStoreId(currentUser.getStoreId());
        couponVO.setStoreName(currentUser.getStoreName());
        couponVO.setPromotionStatus(PromotionStatusEnum.NEW.name());
        Coupon byId = couponService.getById(couponVO.getId());
        if (!currentUser.getStoreId().equals(byId.getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        CouponVO coupon = couponService.updateCoupon(couponVO);
        return ResultUtil.data(coupon);
    }

    @DeleteMapping(value = "/{ids}")
    @ApiOperation(value = "批量删除")
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        AuthUser currentUser = UserContext.getCurrentUser();
        Coupon byId = couponService.getById(ids.get(0));
        if (!currentUser.getStoreId().equals(byId.getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        for (String id : ids) {
            couponService.deleteCoupon(id);
        }
        return ResultUtil.success();
    }

    @ApiOperation(value = "修改优惠券状态")
    @PutMapping("/status")
    public ResultMessage<Object> updateCouponStatus(String couponIds, String promotionStatus) {
        String[] split = couponIds.split(",");
        if (couponService.updateCouponStatus(Arrays.asList(split), PromotionStatusEnum.valueOf(promotionStatus))) {
            return ResultUtil.success(ResultCode.COUPON_EDIT_STATUS_SUCCESS);
        }
        throw new ServiceException(ResultCode.COUPON_EDIT_STATUS_ERROR);
    }
}
