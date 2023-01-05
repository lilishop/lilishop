package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dto.search.CouponSearchParams;
import cn.lili.modules.promotion.entity.vos.CouponVO;
import cn.lili.modules.promotion.service.CouponService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * 店铺端,优惠券接口
 *
 * @author paulG
 * @since 2020/8/28
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
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        queryParam.setStoreId(storeId);
        IPage<CouponVO> coupons = couponService.pageVOFindAll(queryParam, page);
        return ResultUtil.data(coupons);
    }

    @ApiOperation(value = "获取优惠券详情")
    @GetMapping("/{couponId}")
    public ResultMessage<Coupon> getCouponList(@PathVariable String couponId) {
        CouponVO coupon = OperationalJudgment.judgment(couponService.getDetail(couponId));
        return ResultUtil.data(coupon);
    }

    @ApiOperation(value = "添加优惠券")
    @PostMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<CouponVO> addCoupon(@RequestBody CouponVO couponVO) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        couponVO.setStoreId(currentUser.getStoreId());
        couponVO.setStoreName(currentUser.getStoreName());
        if (couponService.savePromotions(couponVO)) {
            return ResultUtil.data(couponVO);
        }
        return ResultUtil.error(ResultCode.COUPON_SAVE_ERROR);
    }

    @PutMapping(consumes = "application/json", produces = "application/json")
    @ApiOperation(value = "修改优惠券")
    public ResultMessage<Coupon> updateCoupon(@RequestBody CouponVO couponVO) {
        OperationalJudgment.judgment(couponService.getById(couponVO.getId()));
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        couponVO.setStoreId(currentUser.getStoreId());
        couponVO.setStoreName(currentUser.getStoreName());
        if (couponService.updatePromotions(couponVO)) {
            return ResultUtil.data(couponVO);
        }
        return ResultUtil.error(ResultCode.COUPON_SAVE_ERROR);
    }

    @DeleteMapping(value = "/{ids}")
    @ApiOperation(value = "批量删除")
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        LambdaQueryWrapper<Coupon> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.in(Coupon::getId, ids);
        queryWrapper.eq(Coupon::getStoreId, storeId);
        List<Coupon> list = couponService.list(queryWrapper);
        List<String> filterIds = list.stream().map(Coupon::getId).collect(Collectors.toList());
        return couponService.removePromotions(filterIds) ? ResultUtil.success() : ResultUtil.error(ResultCode.COUPON_DELETE_ERROR);
    }

    @ApiOperation(value = "修改优惠券状态")
    @PutMapping("/status")
    public ResultMessage<Object> updateCouponStatus(String couponIds, Long startTime, Long endTime) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        String[] split = couponIds.split(",");
        List<String> couponIdList = couponService.list(new LambdaQueryWrapper<Coupon>().in(Coupon::getId, Arrays.asList(split)).eq(Coupon::getStoreId, currentUser.getStoreId())).stream().map(Coupon::getId).collect(Collectors.toList());
        if (couponService.updateStatus(couponIdList, startTime, endTime)) {
            return ResultUtil.success(ResultCode.COUPON_EDIT_STATUS_SUCCESS);
        }
        throw new ServiceException(ResultCode.COUPON_EDIT_STATUS_ERROR);
    }
}
