package cn.lili.controller.manager.order;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.order.entity.dto.OrderReceiptDTO;
import cn.lili.modules.order.order.entity.dto.ReceiptSearchParams;
import cn.lili.modules.order.order.service.ReceiptService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端,发票记录接口
 *
 * @author paulG
 * @since 2020/11/17 4:34 下午
 **/
@RestController
@Tag(name = "管理端,发票记录接口")
@RequestMapping("/manager/trade/receipt")
public class ReceiptManagerController {

    @Autowired
    private ReceiptService receiptService;


    @Operation(description = "获取发票分页信息")
    @Parameter(name = "searchParams", description = "查询参数")
    @Parameter(name = "pageVO", description = "分页参数")
    @GetMapping
    public ResultMessage<IPage<OrderReceiptDTO>> getPage(ReceiptSearchParams searchParams, PageVO pageVO) {
        return ResultUtil.data(this.receiptService.getReceiptData(searchParams, pageVO));
    }

}
