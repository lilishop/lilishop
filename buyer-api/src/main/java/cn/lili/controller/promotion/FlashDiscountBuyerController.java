package cn.lili.controller.promotion;

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

@RestController
@Tag(name = "买家端,限时直降接口")
@RequestMapping("/buyer/promotion/flashDiscount")
public class FlashDiscountBuyerController {

    @Autowired
    private FlashDiscountService flashDiscountService;

    @GetMapping
    @Operation(summary = "分页查询进行中的限时直降")
    public ResultMessage<IPage<FlashDiscount>> page(FlashDiscountSearchParams params, PageVO page) {
        return ResultUtil.data(flashDiscountService.pageFindAll(params, page));
    }

    @GetMapping("/{id}")
    @Operation(summary = "活动详情")
    public ResultMessage<FlashDiscountVO> get(@PathVariable String id) {
        return ResultUtil.data(flashDiscountService.getFlashDiscountVO(id));
    }
}
