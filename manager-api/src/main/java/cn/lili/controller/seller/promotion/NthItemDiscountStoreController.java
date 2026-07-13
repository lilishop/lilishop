package cn.lili.controller.seller.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.NthItemDiscount;
import cn.lili.modules.promotion.entity.dto.search.NthItemDiscountSearchParams;
import cn.lili.modules.promotion.entity.vos.NthItemDiscountVO;
import cn.lili.modules.promotion.service.NthItemDiscountService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;
import java.util.Objects;

@RestController
@Tag(name = "店铺端,第N件优惠接口")
@RequestMapping("/store/promotion/nthItemDiscount")
public class NthItemDiscountStoreController {

    @Autowired
    private NthItemDiscountService nthItemDiscountService;

    @GetMapping
    public ResultMessage<IPage<NthItemDiscount>> page(NthItemDiscountSearchParams params, PageVO page) {
        params.setStoreId(Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId());
        return ResultUtil.data(nthItemDiscountService.pageFindAll(params, page));
    }

    @GetMapping("/{id}")
    public ResultMessage<NthItemDiscountVO> get(@PathVariable String id) {
        return ResultUtil.data(OperationalJudgment.judgment(nthItemDiscountService.getNthItemDiscountVO(id)));
    }

    @PostMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<NthItemDiscountVO> add(@RequestBody @Validated NthItemDiscountVO vo) {
        AuthUser user = Objects.requireNonNull(UserContext.getCurrentUser());
        vo.setStoreId(user.getStoreId());
        vo.setStoreName(user.getStoreName());
        nthItemDiscountService.savePromotions(vo);
        return ResultUtil.data(vo);
    }

    @PutMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<Object> edit(@RequestBody @Validated NthItemDiscountVO vo) {
        OperationalJudgment.judgment(nthItemDiscountService.getById(vo.getId()));
        AuthUser user = Objects.requireNonNull(UserContext.getCurrentUser());
        vo.setStoreId(user.getStoreId());
        vo.setStoreName(user.getStoreName());
        nthItemDiscountService.updatePromotions(vo);
        return ResultUtil.success();
    }

    @DeleteMapping("/{id}")
    public ResultMessage<Object> delete(@PathVariable String id) {
        OperationalJudgment.judgment(nthItemDiscountService.getById(id));
        nthItemDiscountService.removePromotions(Collections.singletonList(id));
        return ResultUtil.success();
    }

    @PutMapping("/status/{id}")
    public ResultMessage<Object> status(@PathVariable String id, Long startTime, Long endTime) {
        OperationalJudgment.judgment(nthItemDiscountService.getById(id));
        nthItemDiscountService.updateStatus(Collections.singletonList(id), startTime, endTime);
        return ResultUtil.success();
    }
}
