package cn.lili.controller.manager.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dto.search.FullDiscountSearchParams;
import cn.lili.modules.promotion.service.FullDiscountService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;

/**
 * 管理端,满额活动接口
 *
 * @author paulG
 * @since 2021/1/12
 **/
@RestController
@Tag(name = "管理端,满额活动接口")
@RequestMapping("/manager/promotion/fullDiscount")
public class FullDiscountManagerController {

    @Autowired
    private FullDiscountService fullDiscountService;

    @Operation(description = "获取满优惠列表")
    @Parameter(name = "searchParams", description = "查询参数", required = true)
    @Parameter(name = "page", description = "分页参数", required = true)
    @GetMapping
    public ResultMessage<IPage<FullDiscount>> getCouponList(FullDiscountSearchParams searchParams, PageVO page) {
        return ResultUtil.data(fullDiscountService.pageFindAll(searchParams, page));
    }

    @Operation(description = "获取满优惠详情")
    @Parameter(name = "id", description = "满额活动ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<FullDiscountVO> getCouponDetail(@PathVariable String id) {
        return ResultUtil.data(fullDiscountService.getFullDiscount(id));
    }

    @Operation(description = "修改满额活动状态")
    @Parameter(name = "id", description = "满额活动ID", required = true)
    @Parameter(name = "promotionStatus", description = "满额活动状态", required = true)
    @PutMapping("/status/{id}")
    public ResultMessage<Object> updateCouponStatus(@PathVariable String id, Long startTime, Long endTime) {
        if (fullDiscountService.updateStatus(Collections.singletonList(id), startTime, endTime)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        return ResultUtil.error(ResultCode.ERROR);
    }
}
           
