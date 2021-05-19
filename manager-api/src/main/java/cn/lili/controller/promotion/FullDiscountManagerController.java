package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.vos.FullDiscountSearchParams;
import cn.lili.modules.promotion.service.FullDiscountService;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,满额活动接口
 *
 * @author paulG
 * @date 2021/1/12
 **/
@RestController
@Api(tags = "管理端,满额活动接口")
@RequestMapping("/manager/promotion/fullDiscount")
public class FullDiscountManagerController {
    @Autowired
    private FullDiscountService fullDiscountService;

    @ApiOperation(value = "获取满优惠列表")
    @GetMapping
    public ResultMessage<IPage<FullDiscountVO>> getCouponList(FullDiscountSearchParams searchParams, PageVO page) {
        page.setNotConvert(true);
        return ResultUtil.data(fullDiscountService.getFullDiscountByPageFromMongo(searchParams, page));
    }

    @ApiOperation(value = "获取满优惠详情")
    @GetMapping("/{id}")
    public ResultMessage<FullDiscountVO> getCouponDetail(@PathVariable String id) {
        return ResultUtil.data(fullDiscountService.getFullDiscount(id));
    }

    @ApiOperation(value = "获取满优惠商品列表")
    @GetMapping("/goods/{id}")
    public ResultMessage<FullDiscountVO> getCouponGoods(@PathVariable String id) {
        return ResultUtil.data(fullDiscountService.getFullDiscount(id));
    }

}
