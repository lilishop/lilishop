package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.CouponActivity;
import cn.lili.modules.promotion.entity.dto.CouponActivityDTO;
import cn.lili.modules.promotion.entity.vos.CouponActivityVO;
import cn.lili.modules.promotion.service.CouponActivityService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;

/**
 * 优惠券活动
 *
 * @author Bulbasaur
 * @since 2021/5/21 7:11 下午
 */
@RestController
@Api(tags = "管理端,优惠券活动接口")
@RequestMapping("/manager/promotion/couponActivity")
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
    @GetMapping("/{couponActivityId}")
    public ResultMessage<CouponActivityVO> getCouponActivity(@PathVariable String couponActivityId) {
        return ResultUtil.data(couponActivityService.getCouponActivityVO(couponActivityId));
    }

    @ApiOperation(value = "添加优惠券活动")
    @PostMapping
    @PutMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<CouponActivity> addCouponActivity(@RequestBody(required = false) CouponActivityDTO couponActivityDTO) {
        if (couponActivityService.savePromotions(couponActivityDTO)) {
            return ResultUtil.data(couponActivityDTO);
        }
        return ResultUtil.error(ResultCode.COUPON_ACTIVITY_SAVE_ERROR);
    }

    @ApiOperation(value = "关闭优惠券活动")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "优惠券活动ID", required = true, dataType = "String", paramType = "path")
    })
    @DeleteMapping("/{id}")
    public ResultMessage<CouponActivity> updateStatus(@PathVariable String id) {
        if (couponActivityService.updateStatus(Collections.singletonList(id), null, null)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }


}
