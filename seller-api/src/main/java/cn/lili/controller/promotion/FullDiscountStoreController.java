package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.vos.FullDiscountSearchParams;
import cn.lili.modules.promotion.service.FullDiscountService;
import cn.lili.modules.order.cart.entity.vo.FullDiscountVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 店铺端,满额活动接口
 *
 * @author paulG
 * @date 2020/8/19
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
        AuthUser currentUser = UserContext.getCurrentUser();
        fullDiscountVO.setStoreId(currentUser.getStoreId());
        fullDiscountVO.setStoreName(currentUser.getStoreName());
        fullDiscountVO.setPromotionStatus(PromotionStatusEnum.NEW.name());
        FullDiscount fullDiscount = fullDiscountService.addFullDiscount(fullDiscountVO);
        return ResultUtil.data(fullDiscount);
    }

    @ApiOperation(value = "通过id获取")
    @GetMapping("/{id}")
    public ResultMessage<FullDiscountVO> get(@PathVariable String id) {
        FullDiscountVO fullDiscount = fullDiscountService.getFullDiscount(id);
        return ResultUtil.data(fullDiscount);
    }

    @ApiOperation(value = "根据条件分页查询满优惠活动")
    @GetMapping
    public ResultMessage<IPage<FullDiscountVO>> getFullDiscountByPage(FullDiscountSearchParams searchParams, PageVO page) {
        String storeId = UserContext.getCurrentUser().getStoreId();
        searchParams.setStoreId(storeId);
        IPage<FullDiscountVO> fullDiscountByPage = fullDiscountService.getFullDiscountByPageFromMongo(searchParams, page);
        return ResultUtil.data(fullDiscountByPage);
    }

    @ApiOperation(value = "修改满优惠活动")
    @PutMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<String> editFullDiscount(@RequestBody FullDiscountVO fullDiscountVO) {
        AuthUser currentUser = UserContext.getCurrentUser();
        fullDiscountVO.setStoreId(currentUser.getStoreId());
        fullDiscountVO.setStoreName(currentUser.getStoreName());
        fullDiscountService.modifyFullDiscount(fullDiscountVO);
        return ResultUtil.success(ResultCode.FULL_DISCOUNT_EDIT_SUCCESS);
    }

    @ApiOperation(value = "删除满优惠活动")
    @DeleteMapping("/{id}")
    public ResultMessage<String> deleteFullDiscount(@PathVariable String id) {
        fullDiscountService.deleteFullDiscount(id);
        return ResultUtil.success(ResultCode.FULL_DISCOUNT_EDIT_DELETE);
    }

}
