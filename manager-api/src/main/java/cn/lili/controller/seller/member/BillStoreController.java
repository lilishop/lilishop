package cn.lili.controller.seller.member;

import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.store.entity.dos.Bill;
import cn.lili.modules.store.entity.dto.BillSearchParams;
import cn.lili.modules.store.entity.vos.BillListVO;
import cn.lili.modules.store.service.BillService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.servlet.http.HttpServletResponse;
import java.util.Objects;

/**
 * 店铺端,结算单接口
 *
 * @author Chopper
 * @since 2020/11/17 4:29 下午
 */
@RestController
@Tag(name = "店铺端,结算单接口")
@RequestMapping("/store/order/bill")
public class BillStoreController {

    @Autowired
    private BillService billService;

    @Autowired
    private StoreFlowService storeFlowService;

    @Operation(summary = "获取结算单分页")
    @GetMapping("/getByPage")
    public ResultMessage<IPage<BillListVO>> getByPage(BillSearchParams billSearchParams) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        billSearchParams.setStoreId(storeId);
        return ResultUtil.data(billService.billPage(billSearchParams));
    }

    @Operation(summary = "通过id获取结算单")
    @Parameter(name = "id", description = "结算单ID", required = true)
    @GetMapping("/get/{id}")
    public ResultMessage<Bill> get(@PathVariable String id) {
        return ResultUtil.data(OperationalJudgment.judgment(billService.getById(id)));
    }

    @Operation(summary = "获取商家结算单流水分页")
    @Parameter(name = "id", description = "结算单ID", required = true)
    @Parameter(name = "flowType", description = "流水类型:PAY、REFUND")
    @GetMapping("/{id}/getStoreFlow")
    public ResultMessage<IPage<StoreFlow>> getStoreFlow(@PathVariable String id, String flowType, PageVO pageVO) {
        OperationalJudgment.judgment(billService.getById(id));
        return ResultUtil.data(storeFlowService.getStoreFlow(id, flowType, pageVO));
    }

    @Operation(summary = "获取商家分销订单流水分页")
    @Parameter(name = "id", description = "结算单ID", required = true)
    @GetMapping("/{id}/getDistributionFlow")
    public ResultMessage<IPage<StoreFlow>> getDistributionFlow(@PathVariable String id, PageVO pageVO) {
        OperationalJudgment.judgment(billService.getById(id));
        return ResultUtil.data(storeFlowService.getDistributionFlow(id, pageVO));
    }

    @Operation(summary = "核对结算单")
    @Parameter(name = "id", description = "结算单ID", required = true)
    @PutMapping("/check/{id}")
    public ResultMessage<Object> examine(@PathVariable String id) {
        OperationalJudgment.judgment(billService.getById(id));
        billService.check(id);
        return ResultUtil.success();
    }

    @Operation(summary = "下载结算单")
    @Parameter(name = "id", description = "结算单ID", required = true)
    @GetMapping("/downLoad/{id}")
    public void downLoadDeliverExcel(@PathVariable String id) {
        OperationalJudgment.judgment(billService.getById(id));
        HttpServletResponse response = ThreadContextHolder.getHttpResponse();
        billService.download(response, id);

    }

}
