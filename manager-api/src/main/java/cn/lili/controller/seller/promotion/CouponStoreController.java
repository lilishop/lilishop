package cn.lili.controller.seller.promotion;

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
import cn.lili.modules.promotion.entity.dto.search.MemberCouponSearchParams;
import cn.lili.modules.promotion.entity.vos.CouponVO;
import cn.lili.modules.promotion.entity.vos.MemberCouponVO;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * 店铺端,优惠券接口
 *
 * @author paulG
 * @since 2020/8/28
 **/
@RestController
@Tag(name = "店铺端,优惠券接口")
@RequestMapping("/store/promotion/coupon")
public class CouponStoreController {

    @Autowired
    private CouponService couponService;


    @Autowired
    private MemberCouponService memberCouponService;

    @Operation(description = "获取优惠券列表")
    @Parameter(name = "queryParam", description = "优惠券查询参数")
    @Parameter(name = "page", description = "分页参数")
    @GetMapping
    public ResultMessage<IPage<CouponVO>> getCouponList(CouponSearchParams queryParam, PageVO page) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        queryParam.setStoreId(storeId);
        IPage<CouponVO> coupons = couponService.pageVOFindAll(queryParam, page);
        return ResultUtil.data(coupons);
    }

    @Operation(description = "获取优惠券详情")
    @Parameter(name = "couponId", description = "优惠券ID", required = true)
    @GetMapping("/{couponId}")
    public ResultMessage<Coupon> getCouponList(@PathVariable String couponId) {
        CouponVO coupon = OperationalJudgment.judgment(couponService.getDetail(couponId));
        return ResultUtil.data(coupon);
    }

    @Operation(description = "添加优惠券")
    @Parameter(name = "couponVO", description = "优惠券VO", required = true)
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
    @Operation(description = "修改优惠券")
    @Parameter(name = "couponVO", description = "优惠券VO", required = true)
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

    @Operation(description = "批量删除优惠券")
    @DeleteMapping("/{ids}")
    @Parameter(name = "ids", description = "优惠券ID列表", required = true)
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        List<String> filterIds = couponService.filterCouponIdsByStoreId(ids, storeId);
        return couponService.removePromotions(filterIds) ? ResultUtil.success() : ResultUtil.error(ResultCode.COUPON_DELETE_ERROR);
    }

    @Operation(description = "获取优惠券领取详情")
    @Parameter(name = "searchParams", description = "优惠券领取查询参数")
    @Parameter(name = "page", description = "分页参数")
    @GetMapping("/received")
    public ResultMessage<IPage<MemberCouponVO>> getReceiveByPage(MemberCouponSearchParams searchParams,
                                                                 PageVO page) {
        searchParams.setStoreId(Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId());
        IPage<MemberCouponVO> result = memberCouponService.getMemberCouponsPage(PageUtil.initPage(page), searchParams);
        return ResultUtil.data(result);
    }

    @Operation(description = "修改优惠券状态")
    @Parameter(name = "couponIds", description = "优惠券ID列表", required = true)
    @Parameter(name = "startTime", description = "优惠券开始时间")
    @Parameter(name = "endTime", description = "优惠券结束时间")
    @PutMapping("/status")
    public ResultMessage<Object> updateCouponStatus(String couponIds, Long startTime, Long endTime) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        String[] split = couponIds.split(",");
        List<String> couponIdList = couponService.filterCouponIdsByStoreId(Arrays.asList(split), currentUser.getStoreId());
        if (couponService.updateStatus(couponIdList, startTime, endTime)) {
            return ResultUtil.success(ResultCode.COUPON_EDIT_STATUS_SUCCESS);
        }
        throw new ServiceException(ResultCode.COUPON_EDIT_STATUS_ERROR);
    }
}
