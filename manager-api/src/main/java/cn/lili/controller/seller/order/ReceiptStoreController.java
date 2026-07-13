package cn.lili.controller.seller.order;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.order.entity.dos.Receipt;
import cn.lili.modules.order.order.entity.dto.OrderReceiptDTO;
import cn.lili.modules.order.order.entity.dto.ReceiptInvoicingDTO;
import cn.lili.modules.order.order.entity.dto.ReceiptSearchParams;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.service.ReceiptService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import jakarta.validation.Valid;

import java.util.Objects;

/**
 * 店铺端,发票接口
 *
 * @author Bulbasaur
 * @since 2020/11/28 14:09
 **/
@RestController
@Tag(name = "店铺端,发票接口")
@RequestMapping("/store/trade/receipt")
public class ReceiptStoreController {

    @Autowired
    private ReceiptService receiptService;

    @Autowired
    private OrderService orderService;

    @Operation(description = "分页获取")
    @Parameter(name = "page", description = "分页参数")
    @Parameter(name = "receiptSearchParams", description = "发票查询参数")
    @GetMapping
    public ResultMessage<IPage<OrderReceiptDTO>> getByPage(PageVO page, ReceiptSearchParams receiptSearchParams) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        receiptSearchParams.setStoreId(storeId);
        return ResultUtil.data(receiptService.getReceiptData(receiptSearchParams, page));
    }

    @Operation(description = "通过id获取")
    @Parameter(name = "id", description = "发票ID", required = true)
    @GetMapping("/get/{id}")
    public ResultMessage<Receipt> get(@PathVariable String id) {
        return ResultUtil.data(OperationalJudgment.judgment(receiptService.getById(id)));
    }

    @Operation(description = "开发票")
    @Parameter(name = "id", description = "发票ID", required = true)
    @PostMapping("/{id}/invoicing")
    public ResultMessage<Receipt> invoicing(@PathVariable String id, @Valid ReceiptInvoicingDTO receiptInvoicingDTO) {
        OperationalJudgment.judgment(receiptService.getById(id));
        return ResultUtil.data(receiptService.invoicing(id, receiptInvoicingDTO));
    }

    @Operation(description = "通过订单编号获取")
    @Parameter(name = "orderSn", description = "订单编号", required = true)
    @GetMapping("/get/orderSn/{orderSn}")
    public ResultMessage<Receipt> getByOrderSn(@PathVariable String orderSn) {
        OperationalJudgment.judgment(orderService.getBySn(orderSn));
        return ResultUtil.data(receiptService.getByOrderSn(orderSn));
    }

}
