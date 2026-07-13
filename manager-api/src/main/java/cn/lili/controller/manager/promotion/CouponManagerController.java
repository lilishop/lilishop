package cn.lili.controller.manager.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import cn.lili.modules.promotion.entity.dto.search.CouponSearchParams;
import cn.lili.modules.promotion.entity.dto.search.MemberCouponSearchParams;
import cn.lili.modules.promotion.entity.vos.CouponVO;
import cn.lili.modules.promotion.entity.vos.MemberCouponVO;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.modules.promotion.tools.PromotionTools;
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
 * 管理端,优惠券接口
 *
 * @author paulG
 * @since 2020/10/9
 **/
@RestController
@Tag(name = "管理端,优惠券接口")
@RequestMapping("/manager/promotion/coupon")
public class CouponManagerController {
    @Autowired
    private CouponService couponService;
    @Autowired
    private MemberCouponService memberCouponService;

    @Operation(description = "获取优惠券列表")
    @Parameter(name = "queryParam", description = "查询参数", required = true)
    @Parameter(name = "page", description = "分页参数", required = true)
    @GetMapping
    public ResultMessage<IPage<CouponVO>> getCouponList(CouponSearchParams queryParam, PageVO page) {
        if (queryParam.getStoreId() == null) {
            queryParam.setStoreId(PromotionTools.PLATFORM_ID);
        }
        return ResultUtil.data(couponService.pageVOFindAll(queryParam, page));
    }

    @Operation(description = "获取优惠券详情")
    @Parameter(name = "couponId", description = "优惠券ID", required = true)
    @GetMapping("/{couponId}")
    public ResultMessage<CouponVO> getCoupon(@PathVariable String couponId) {
        CouponVO coupon = couponService.getDetail(couponId);
        return ResultUtil.data(coupon);
    }

    @Operation(description = "添加优惠券")
    @Parameter(name = "couponVO", description = "优惠券参数", required = true)
    @PostMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<CouponVO> addCoupon(@RequestBody CouponVO couponVO) {
        this.setStoreInfo(couponVO);
        couponService.savePromotions(couponVO);
        return ResultUtil.data(couponVO);
    }

    @Operation(description = "修改优惠券")
    @Parameter(name = "couponVO", description = "优惠券参数", required = true)
    @PutMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<Coupon> updateCoupon(@RequestBody CouponVO couponVO) {
        this.setStoreInfo(couponVO);
        Coupon coupon = couponService.getById(couponVO.getId());
        couponService.updatePromotions(couponVO);
        return ResultUtil.data(coupon);
    }

    @Operation(description = "修改优惠券状态")
    @Parameter(name = "couponIds", description = "优惠券ID", required = true)
    @Parameter(name = "startTime", description = "开始时间", required = true)
    @Parameter(name = "endTime", description = "结束时间", required = true)
    @PutMapping("/status")
    public ResultMessage<Object> updateCouponStatus(String couponIds, Long startTime, Long endTime) {
        String[] split = couponIds.split(",");
        if (couponService.updateStatus(Arrays.asList(split), startTime, endTime)) {
            return ResultUtil.success(ResultCode.COUPON_EDIT_STATUS_SUCCESS);
        }
        throw new ServiceException(ResultCode.COUPON_EDIT_STATUS_ERROR);
    }

    @Operation(description = "批量删除")
    @Parameter(name = "ids", description = "优惠券ID", required = true)
    @DeleteMapping("/{ids}")
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        couponService.removePromotions(ids);
        return ResultUtil.success();
    }

    @Operation(description = "客户优惠券作废")
    @Parameter(name = "id", description = "客户优惠券ID", required = true)
    @PutMapping("/member/cancellation/{id}")
    public ResultMessage<Object> cancellation(@PathVariable String id) {
        AuthUser currentUser =  Objects.requireNonNull(UserContext.getCurrentUser());
        memberCouponService.cancellation(currentUser.getId(), id);
        return ResultUtil.success(ResultCode.COUPON_CANCELLATION_SUCCESS);
    }

    @Operation(description = "根据优惠券id券分页获取客户领详情")
    @Parameter(name = "id", description = "优惠券ID", required = true)
    @Parameter(name = "page", description = "分页参数", required = true)
    @GetMapping("/member/{id}")
    public ResultMessage<IPage<MemberCoupon>> getByPage(@PathVariable String id,
                                                        PageVO page) {
        return ResultUtil.data(memberCouponService.pageByCouponId(id, page));
    }

    @Operation(description = "获取优惠券领取详情")
    @Parameter(name = "searchParams", description = "查询参数", required = true)
    @Parameter(name = "page", description = "分页参数", required = true)
    @GetMapping("/received")
    public ResultMessage<IPage<MemberCouponVO>> getReceiveByPage(MemberCouponSearchParams searchParams,
                                                                 PageVO page) {
        IPage<MemberCouponVO> result = memberCouponService.getMemberCouponsPage(PageUtil.initPage(page), searchParams);
        return ResultUtil.data(result);
    }

    private void setStoreInfo(CouponVO couponVO) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        couponVO.setStoreId(PromotionTools.PLATFORM_ID);
        couponVO.setStoreName(PromotionTools.PLATFORM_NAME);
    }

}
