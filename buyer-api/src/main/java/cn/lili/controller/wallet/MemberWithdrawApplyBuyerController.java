package cn.lili.controller.wallet;


import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.wallet.entity.dos.MemberWithdrawApply;
import cn.lili.modules.wallet.entity.vo.MemberWithdrawApplyQueryVO;
import cn.lili.modules.wallet.entity.vo.WechatTransferInfoVO;
import cn.lili.modules.wallet.service.MemberWithdrawApplyService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * 买家端,余额提现记录接口
 *
 * @author pikachu
 * @since 2020/11/16 10:07 下午
 */
@RestController
@Tag(name = "买家端,余额提现记录接口")
@RequestMapping("/buyer/member/withdrawApply")
public class MemberWithdrawApplyBuyerController {
    @Autowired
    private MemberWithdrawApplyService memberWithdrawApplyService;


    @Operation(description = "分页获取提现记录")
    @Parameter(name = "page", description = "分页参数")
    @Parameter(name = "memberWithdrawApplyQueryVO", description = "查询参数")
    @GetMapping
    public ResultMessage<IPage<MemberWithdrawApply>> getByPage(PageVO page, MemberWithdrawApplyQueryVO memberWithdrawApplyQueryVO) {
        memberWithdrawApplyQueryVO.setMemberId(UserContext.getCurrentUser().getId());
        //构建查询 返回数据
        IPage<MemberWithdrawApply> memberWithdrawApplyPage = memberWithdrawApplyService.getMemberWithdrawPage(page, memberWithdrawApplyQueryVO);
        return ResultUtil.data(memberWithdrawApplyPage);
    }

    @Operation(description = "查询微信转账信息")
    @Parameter(name = "id", description = "申请提现ID", required = true)
    @GetMapping("/wechat/transfer/{id}")
    public ResultMessage<WechatTransferInfoVO> getWechatTransferInfo(@PathVariable String id) {
        return ResultUtil.data(memberWithdrawApplyService.getWechatTransferInfo(id));
    }



}
