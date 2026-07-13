package cn.lili.controller.seller.procurement;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.procurement.entity.dos.StockReason;
import cn.lili.modules.procurement.service.StockReasonService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 店铺端出入库原因接口
 * 提供原因的分页列表
 * @author Bulbasaur
 * @since 2025-12-18
 */
@Tag(name = "店铺端,出入库原因接口")
@RestController
@RequestMapping("/store/procurement/reason")
public class StockReasonStoreController {

    @Autowired
    private StockReasonService stockReasonService;

    @Operation(summary = "出入库原因列表")
    @GetMapping
    public ResultMessage<IPage<StockReason>> page(String reason, String category, PageVO pageVO) {
        Page<StockReason> page = new Page<>(pageVO.getPageNumber(), pageVO.getPageSize());
        return ResultUtil.data(stockReasonService.page(reason, category, page));
    }
}
