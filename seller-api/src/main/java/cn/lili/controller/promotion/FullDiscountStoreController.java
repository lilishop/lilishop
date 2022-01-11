package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dto.search.FullDiscountSearchParams;
import cn.lili.modules.promotion.service.FullDiscountService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;
import java.util.Objects;

/**
 * 店铺端,满额活动接口
 *
 * @author paulG
 * @since 2020/8/19
 **/
@RestController
@Api(tags = "店铺端,满额活动接口")
@RequestMapping("/store/promotion/fullDiscount")
public class FullDiscountStoreController {

    @Autowired
    private FullDiscountService fullDiscountService;

    @ApiOperation(value = "新增满优惠活动")
    @PostMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<FullDiscount> addFullDiscount(@RequestBody FullDiscountVO fullDiscountVO) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        fullDiscountVO.setStoreId(currentUser.getStoreId());
        fullDiscountVO.setStoreName(currentUser.getStoreName());
        if (!fullDiscountService.savePromotions(fullDiscountVO)) {
            return ResultUtil.error(ResultCode.PINTUAN_ADD_ERROR);
        }
        return ResultUtil.data(fullDiscountVO);
    }

    @ApiOperation(value = "通过id获取")
    @GetMapping("/{id}")
    public ResultMessage<FullDiscountVO> get(@PathVariable String id) {
        FullDiscountVO fullDiscount = OperationalJudgment.judgment(fullDiscountService.getFullDiscount(id));
        return ResultUtil.data(fullDiscount);
    }

    @ApiOperation(value = "根据条件分页查询满优惠活动")
    @GetMapping
    public ResultMessage<IPage<FullDiscount>> getFullDiscountByPage(FullDiscountSearchParams searchParams, PageVO page) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        searchParams.setStoreId(storeId);
        IPage<FullDiscount> fullDiscountByPage = fullDiscountService.pageFindAll(searchParams, page);
        return ResultUtil.data(fullDiscountByPage);
    }

    @ApiOperation(value = "修改满优惠活动")
    @PutMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<String> editFullDiscount(@RequestBody FullDiscountVO fullDiscountVO) {
        OperationalJudgment.judgment(fullDiscountService.getFullDiscount(fullDiscountVO.getId()));
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        fullDiscountVO.setStoreId(currentUser.getStoreId());
        fullDiscountVO.setStoreName(currentUser.getStoreName());
        if (!fullDiscountService.updatePromotions(fullDiscountVO)) {
            return ResultUtil.error(ResultCode.PINTUAN_EDIT_ERROR);
        }
        return ResultUtil.success(ResultCode.FULL_DISCOUNT_EDIT_SUCCESS);
    }

    @ApiOperation(value = "删除满优惠活动")
    @DeleteMapping("/{id}")
    public ResultMessage<String> deleteFullDiscount(@PathVariable String id) {
        OperationalJudgment.judgment(fullDiscountService.getById(id));
        fullDiscountService.removePromotions(Collections.singletonList(id));
        return ResultUtil.success(ResultCode.FULL_DISCOUNT_EDIT_DELETE);
    }


    @ApiOperation(value = "修改满额活动状态")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "满额活动ID", required = true, paramType = "path"),
            @ApiImplicitParam(name = "promotionStatus", value = "满额活动状态", required = true, paramType = "path")
    })
    @PutMapping("/status/{id}")
    public ResultMessage<Object> updateCouponStatus(@PathVariable String id, Long startTime, Long endTime) {
        OperationalJudgment.judgment(fullDiscountService.getFullDiscount(id));
        if (fullDiscountService.updateStatus(Collections.singletonList(id), startTime, endTime)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

}
