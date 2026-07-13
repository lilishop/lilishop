package cn.lili.controller.manager.promotion;

import cn.lili.common.enums.ResultUtil;
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
import org.springframework.web.bind.annotation.*;

import java.util.Collections;

@RestController
@Tag(name = "管理端,限时直降接口")
@RequestMapping("/manager/promotion/flashDiscount")
public class FlashDiscountManagerController {

    @Autowired
    private FlashDiscountService flashDiscountService;

    @GetMapping
    @Operation(summary = "分页查询")
    public ResultMessage<IPage<FlashDiscount>> page(FlashDiscountSearchParams params, PageVO page) {
        return ResultUtil.data(flashDiscountService.pageFindAll(params, page));
    }

    @GetMapping("/{id}")
    @Operation(summary = "详情")
    public ResultMessage<FlashDiscountVO> get(@PathVariable String id) {
        return ResultUtil.data(flashDiscountService.getFlashDiscountVO(id));
    }

    @PutMapping("/status/{id}")
    @Operation(summary = "修改状态")
    public ResultMessage<Object> status(@PathVariable String id, Long startTime, Long endTime) {
        flashDiscountService.updateStatus(Collections.singletonList(id), startTime, endTime);
        return ResultUtil.success();
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "删除")
    public ResultMessage<Object> delete(@PathVariable String id) {
        flashDiscountService.removePromotions(Collections.singletonList(id));
        return ResultUtil.success();
    }
}
