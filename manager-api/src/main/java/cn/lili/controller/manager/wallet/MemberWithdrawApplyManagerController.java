package cn.lili.controller.manager.wallet;


import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.wallet.entity.dos.MemberWithdrawApply;
import cn.lili.modules.wallet.entity.vo.MemberWithdrawApplyQueryVO;
import cn.lili.modules.wallet.service.MemberWithdrawApplyService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * 管理端,余额提现记录接口
 *
 * @author pikachu
 * @since 2020/11/16 10:07 下午
 */
@RestController
@Tag(name = "管理端,余额提现记录接口")
@RequestMapping("/manager/wallet/withdrawApply")
public class MemberWithdrawApplyManagerController {
    @Autowired
    private MemberWithdrawApplyService memberWithdrawApplyService;


    @Operation(summary = "分页获取提现记录")
    @Parameter(name = "page", description = "分页参数", required = true)
    @Parameter(name = "memberWithdrawApplyQueryVO", description = "提现记录查询参数", required = true)
    @GetMapping
    public ResultMessage<IPage<MemberWithdrawApply>> getByPage(PageVO page, MemberWithdrawApplyQueryVO memberWithdrawApplyQueryVO) {
        //构建查询 返回数据
        IPage<MemberWithdrawApply> memberWithdrawApplyPage = memberWithdrawApplyService.getMemberWithdrawPage(page, memberWithdrawApplyQueryVO);
        return ResultUtil.data(memberWithdrawApplyPage);
    }


    @PreventDuplicateSubmissions
    @Operation(summary = "提现申请审核")
    @Parameter(name = "applyId", description = "审核记录id", required = true)
    @Parameter(name = "result", description = "审核结果", required = true)
    @Parameter(name = "remark", description = "审核备注")
    @PostMapping
    public ResultMessage<Boolean> audit(String applyId, Boolean result, String remark) {
        return ResultUtil.data(memberWithdrawApplyService.audit(applyId, result, remark));
    }

}
