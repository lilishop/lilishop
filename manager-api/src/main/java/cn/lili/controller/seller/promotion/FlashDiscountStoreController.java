package cn.lili.controller.seller.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.FlashDiscount;
import cn.lili.modules.promotion.entity.dto.search.FlashDiscountSearchParams;
import cn.lili.modules.promotion.entity.vos.FlashDiscountVO;
import cn.lili.modules.promotion.service.FlashDiscountService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;
import java.util.Objects;

@RestController
@Tag(name = "店铺端,限时直降接口")
@RequestMapping("/store/promotion/flashDiscount")
public class FlashDiscountStoreController {

    @Autowired
    private FlashDiscountService flashDiscountService;

    @GetMapping
    @Operation(description = "分页查询限时直降")
    public ResultMessage<IPage<FlashDiscount>> page(FlashDiscountSearchParams params, PageVO page) {
        params.setStoreId(Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId());
        return ResultUtil.data(flashDiscountService.pageFindAll(params, page));
    }

    @GetMapping("/{id}")
    @Operation(description = "获取详情")
    public ResultMessage<FlashDiscountVO> get(@PathVariable String id) {
        return ResultUtil.data(OperationalJudgment.judgment(flashDiscountService.getFlashDiscountVO(id)));
    }

    @PostMapping(consumes = "application/json", produces = "application/json")
    @Operation(description = "新增")
    public ResultMessage<FlashDiscountVO> add(@RequestBody @Validated FlashDiscountVO vo) {
        AuthUser user = Objects.requireNonNull(UserContext.getCurrentUser());
        vo.setStoreId(user.getStoreId());
        vo.setStoreName(user.getStoreName());
        flashDiscountService.savePromotions(vo);
        return ResultUtil.data(vo);
    }

    @PutMapping(consumes = "application/json", produces = "application/json")
    @Operation(description = "修改")
    public ResultMessage<Object> edit(@RequestBody @Validated FlashDiscountVO vo) {
        OperationalJudgment.judgment(flashDiscountService.getById(vo.getId()));
        AuthUser user = Objects.requireNonNull(UserContext.getCurrentUser());
        vo.setStoreId(user.getStoreId());
        vo.setStoreName(user.getStoreName());
        flashDiscountService.updatePromotions(vo);
        return ResultUtil.success();
    }

    @DeleteMapping("/{id}")
    @Operation(description = "删除")
    public ResultMessage<Object> delete(@PathVariable String id) {
        OperationalJudgment.judgment(flashDiscountService.getById(id));
        flashDiscountService.removePromotions(Collections.singletonList(id));
        return ResultUtil.success();
    }

    @PutMapping("/status/{id}")
    @Operation(description = "修改状态")
    public ResultMessage<Object> status(@PathVariable String id, Long startTime, Long endTime) {
        OperationalJudgment.judgment(flashDiscountService.getById(id));
        flashDiscountService.updateStatus(Collections.singletonList(id), startTime, endTime);
        return ResultUtil.success();
    }
}
