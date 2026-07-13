package cn.lili.controller.manager.wallet;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.wallet.entity.dto.MemberWalletUpdateDTO;
import cn.lili.modules.wallet.entity.enums.DepositServiceTypeEnum;
import cn.lili.modules.wallet.entity.vo.MemberWalletVO;
import cn.lili.modules.wallet.service.MemberWalletService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 管理端,预存款接口
 *
 * @author pikachu
 * @since 2020/11/16 10:07 下午
 */
@RestController
@Tag(name = "管理端,预存款接口")
@RequestMapping("/manager/wallet/wallet")
public class MemberWalletManagerController {
    @Autowired
    private MemberWalletService memberWalletService;

    @GetMapping
    @Operation(summary = "查询客户预存款余额")
    @Parameter(name = "memberId", description = "客户ID", required = true)
    public ResultMessage<MemberWalletVO> get(@RequestParam("memberId") String memberId) {
        return ResultUtil.data(memberWalletService.getMemberWallet(memberId));
    }

    @PutMapping("/increase")
    @Operation(summary = "增加用户余额")
    @Parameter(name = "memberId", description = "客户ID", required = true)
    @Parameter(name = "rechargeMoney", description = "充值金额", required = true)
    public ResultMessage<Object> increase(String memberId ,Double rechargeMoney) {

        MemberWalletUpdateDTO memberWalletUpdateDTO=new MemberWalletUpdateDTO(rechargeMoney,memberId,"运营后台手动充值:"+rechargeMoney, DepositServiceTypeEnum.WALLET_RECHARGE.name());
        if(memberWalletService.increase(memberWalletUpdateDTO)){
            return ResultUtil.success();
        }
        return ResultUtil.error();
    }




}
