package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.CouponActivity;
import cn.lili.modules.promotion.entity.dto.CouponActivityDTO;
import cn.lili.modules.promotion.entity.vos.CouponActivityVO;
import cn.lili.modules.promotion.service.CouponActivityService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 优惠券活动
 *
 * @author Bulbasaur
 * @date: 2021/5/21 7:11 下午
 */
@RestController
@Api(tags = "管理端,优惠券接口")
@RequestMapping("/manager/promotion/coupon")
public class CouponActivityManagerController {

    @Autowired
    private CouponActivityService couponActivityService;

    @ApiOperation(value = "获取优惠券活动分页")
    @GetMapping
    public ResultMessage<IPage<CouponActivity>> getCouponActivityPage(PageVO page) {
        return ResultUtil.data(couponActivityService.page(PageUtil.initPage(page)));
    }

    @ApiOperation(value = "获取优惠券活动")
    @ApiImplicitParam(name = "couponActivityId", value = "优惠券活动ID", required = true, paramType = "path")
    @GetMapping("/getCouponActivity/{couponActivityId}")
    public ResultMessage<CouponActivityVO> getCouponActivity(@PathVariable String couponActivityId) {
        return ResultUtil.data(couponActivityService.getCouponActivityVO(couponActivityId));
    }

    @ApiOperation(value = "添加优惠券活动")
    @PostMapping("/add")
    public ResultMessage<CouponActivity> addCouponActivity(@Validated CouponActivityDTO couponActivityDTO) {
        return ResultUtil.data(couponActivityService.addCouponActivity(couponActivityDTO));
    }
}
