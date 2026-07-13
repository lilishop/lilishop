package cn.lili.controller.manager.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.NthItemDiscount;
import cn.lili.modules.promotion.entity.dto.search.NthItemDiscountSearchParams;
import cn.lili.modules.promotion.entity.vos.NthItemDiscountVO;
import cn.lili.modules.promotion.service.NthItemDiscountService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;

@RestController
@Tag(name = "管理端,第N件优惠接口")
@RequestMapping("/manager/promotion/nthItemDiscount")
public class NthItemDiscountManagerController {

    @Autowired
    private NthItemDiscountService nthItemDiscountService;

    @GetMapping
    public ResultMessage<IPage<NthItemDiscount>> page(NthItemDiscountSearchParams params, PageVO page) {
        return ResultUtil.data(nthItemDiscountService.pageFindAll(params, page));
    }

    @GetMapping("/{id}")
    public ResultMessage<NthItemDiscountVO> get(@PathVariable String id) {
        return ResultUtil.data(nthItemDiscountService.getNthItemDiscountVO(id));
    }

    @PutMapping("/status/{id}")
    public ResultMessage<Object> status(@PathVariable String id, Long startTime, Long endTime) {
        nthItemDiscountService.updateStatus(Collections.singletonList(id), startTime, endTime);
        return ResultUtil.success();
    }

    @DeleteMapping("/{id}")
    public ResultMessage<Object> delete(@PathVariable String id) {
        nthItemDiscountService.removePromotions(Collections.singletonList(id));
        return ResultUtil.success();
    }
}
