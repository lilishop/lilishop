package cn.lili.controller.wallet;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.verification.entity.enums.VerificationEnums;
import cn.lili.modules.verification.service.VerificationService;
import cn.lili.modules.wallet.entity.dos.MemberWallet;
import cn.lili.modules.wallet.entity.vo.MemberWalletVO;
import cn.lili.modules.wallet.service.MemberWalletService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.Pattern;

/**
 * 买家端,会员余额接口
 *
 * @author pikachu
 * @since 2020/11/16 10:07 下午
 */
@RestController
@Api(tags = "买家端,会员余额接口")
@RequestMapping("/buyer/members/wallet")
public class MemberWalletBuyerController {

    /**
     * 会员
     */
    @Autowired
    private MemberService memberService;
    /**
     * 会员余额
     */
    @Autowired
    private MemberWalletService memberWalletService;
    /**
     * 验证码
     */
    @Autowired
    private VerificationService verificationService;

    @GetMapping
    @ApiOperation(value = "查询会员预存款余额")
    public ResultMessage<MemberWalletVO> get() {
        AuthUser authUser = UserContext.getCurrentUser();
        if (authUser != null) {
            return ResultUtil.data(memberWalletService.getMemberWallet(authUser.getId()));
        }
        throw new ServiceException(ResultCode.USER_NOT_LOGIN);
    }

    @PostMapping(value = "/set-password")
    @ApiOperation(value = "设置支付密码")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "password", value = "支付密码", required = true, dataType = "String", paramType = "query")
    })
    public ResultMessage<Object> setPassword(String password, @RequestHeader String uuid) {
        AuthUser authUser = UserContext.getCurrentUser();
        //校验当前用户是否存在
        Member member = memberService.getById(authUser.getId());
        if (member == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        //校验验证码
        if (verificationService.check(uuid, VerificationEnums.WALLET_PASSWORD)) {
            memberWalletService.setMemberWalletPassword(member, password);
            throw new ServiceException(ResultCode.SUCCESS);
        } else {
            throw new ServiceException(ResultCode.VERIFICATION_ERROR);
        }

    }

    @PostMapping(value = "/update-password/ordinary")
    @ApiOperation(value = "普通方式进行支付密码的修改")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "oldPassword", value = "旧支付密码", required = true, dataType = "String", paramType = "query"),
            @ApiImplicitParam(name = "newPassword", value = "新支付密码", required = true, dataType = "String", paramType = "query")
    })
    public ResultMessage updatePassword(@RequestParam @Pattern(regexp = "[a-fA-F0-9]{32}", message = "旧密码格式不正确") String oldPassword,
                                        @RequestParam @Pattern(regexp = "[a-fA-F0-9]{32}", message = "新密码格式不正确") String newPassword) {
        AuthUser authUser = UserContext.getCurrentUser();
        //校验当前用户是否存在
        Member member = memberService.getById(authUser.getId());
        if (member == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        MemberWallet memberWallet = this.memberWalletService.getOne(new QueryWrapper<MemberWallet>().eq("member_id", member.getId()));
        //校验新旧密码是否一致
        if (memberWallet != null) {
            if (!new BCryptPasswordEncoder().matches(oldPassword, memberWallet.getWalletPassword())) {
                throw new ServiceException(ResultCode.USER_OLD_PASSWORD_ERROR);
            }
            this.memberWalletService.setMemberWalletPassword(member, newPassword);
            return ResultUtil.data("修改成功");
        } else {
            throw new ServiceException(ResultCode.WALLET_NOT_EXIT_ERROR);
        }
    }


    @GetMapping(value = "/check")
    @ApiOperation(value = "检测会员是否设置过支付密码,会员中心设置或者修改密码时使用")
    public Boolean checkPassword() {
        return memberWalletService.checkPassword();
    }


    @PostMapping(value = "/withdrawal")
    @ApiOperation(value = "会员中心余额提现")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "price", value = "提现金额", required = true, dataType = "double", paramType = "query")
    })
    public ResultMessage<Boolean> withdrawal(@Max(value = 9999, message = "充值金额单次最多允许提现9999元") @Min(value = 1, message = "充值金额单次最少提现金额为1元") Double price) {
        return ResultUtil.data(memberWalletService.applyWithdrawal(price));
    }

}
