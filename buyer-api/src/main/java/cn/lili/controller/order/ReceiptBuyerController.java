package cn.lili.controller.order;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.order.entity.dos.Receipt;
import cn.lili.modules.order.order.entity.dto.OrderReceiptDTO;
import cn.lili.modules.order.order.entity.dto.ReceiptSearchParams;
import cn.lili.modules.order.order.service.ReceiptService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.Objects;

/**
 * 买家端,发票接口
 *
 * @author paulG
 * @since 2021/1/12
 **/
@RestController
@Tag(name = "买家端,发票接口")
@RequestMapping("/buyer/trade/receipt")
public class ReceiptBuyerController {

    @Autowired
    private ReceiptService receiptService;

    @Operation(summary = "获取发票详情")
    @GetMapping("/{id}")
    public ResultMessage<Receipt> getDetail(@PathVariable String id) {
        Receipt receipt = this.receiptService.getDetail(id);
        if (receipt == null) {
            throw new ServiceException(ResultCode.USER_RECEIPT_NOT_EXIST);
        }
        // 安全防护：核验发票所属人与当前登录用户一致，彻底杜绝遍历 ID 查看他人敏感发票信息的水平越权
        OperationalJudgment.judgment(receipt);
        return ResultUtil.data(receipt);
    }

    @Operation(summary = "获取发票分页信息")
    @GetMapping
    public ResultMessage<IPage<OrderReceiptDTO>> getPage(ReceiptSearchParams searchParams, PageVO pageVO) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        // 安全防护：强制绑定当前登录用户的 memberId，防止前端传空或伪造 memberId 导出全平台其他用户发票
        searchParams.setMemberId(currentUser.getId());
        return ResultUtil.data(this.receiptService.getReceiptData(searchParams, pageVO));
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "保存发票信息")
    @PostMapping
    public ResultMessage<Receipt> save(@Valid Receipt receipt) {
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        // 如果是修改已有发票，先核验数据库中原发票是否属于当前登录用户，防止越权篡改他人发票
        if (receipt.getId() != null) {
            Receipt dbReceipt = receiptService.getById(receipt.getId());
            if (dbReceipt != null) {
                OperationalJudgment.judgment(dbReceipt);
            }
        }
        // 强制使用当前已鉴权的用户身份赋值，杜绝客户端伪造 memberId
        receipt.setMemberId(currentUser.getId());
        receipt.setMemberName(currentUser.getUsername());
        return ResultUtil.data(receiptService.saveReceipt(receipt));
    }

}
