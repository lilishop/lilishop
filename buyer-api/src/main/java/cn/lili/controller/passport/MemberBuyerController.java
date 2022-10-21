package cn.lili.controller.passport;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.MemberEditDTO;
import cn.lili.modules.member.entity.enums.QRCodeLoginSessionStatusEnum;
import cn.lili.modules.member.entity.vo.QRLoginResultVo;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.sms.SmsUtil;
import cn.lili.modules.verification.entity.enums.VerificationEnums;
import cn.lili.modules.verification.service.VerificationService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.async.DeferredResult;

import javax.validation.constraints.NotNull;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;


/**
 * 买家端,会员接口
 *
 * @author Chopper
 * @since 2020/11/16 10:07 下午
 */
@Slf4j
@RestController
@Api(tags = "买家端,会员接口")
@RequestMapping("/buyer/passport/member")
public class MemberBuyerController {

    @Autowired
    private MemberService memberService;
    @Autowired
    private SmsUtil smsUtil;
    @Autowired
    private VerificationService verificationService;


    @ApiOperation(value = "web-获取登录二维码")
    @PostMapping(value = "/pc_session", produces = "application/json;charset=UTF-8")
    public ResultMessage<Object> createPcSession() {
        return ResultUtil.data(memberService.createPcSession());
    }


    /**
     * 长轮询：参考nacos
     *
     * @param token
     * @param beforeSessionStatus 上次记录的session状态
     * @return
     */
    @ApiOperation(value = "web-二维码登录")
    @PostMapping(value = "/session_login/{token}", produces = "application/json;charset=UTF-8")
    public Object loginWithSession(@PathVariable("token") String token, Integer beforeSessionStatus) {
        log.info("receive login with session key {}", token);
        ResponseEntity<ResultMessage<Object>> timeoutResponseEntity =
                new ResponseEntity<>(ResultUtil.error(ResultCode.ERROR), HttpStatus.OK);
        int timeoutSecond = 20;
        DeferredResult<ResponseEntity<Object>> deferredResult = new DeferredResult<>(timeoutSecond * 1000L, timeoutResponseEntity);
        CompletableFuture.runAsync(() -> {
            try {
                int i = 0;
                while (i < timeoutSecond) {
                    QRLoginResultVo queryResult = memberService.loginWithSession(token);
                    int status = queryResult.getStatus();
                    if (status == beforeSessionStatus
                            && (QRCodeLoginSessionStatusEnum.WAIT_SCANNING.getCode() == status
                            || QRCodeLoginSessionStatusEnum.SCANNING.getCode() == status)) {
                        //睡眠一秒种，继续等待结果
                        TimeUnit.SECONDS.sleep(1);
                    } else {
                        deferredResult.setResult(new ResponseEntity<>(ResultUtil.data(queryResult), HttpStatus.OK));
                        break;
                    }
                    i++;
                }
            } catch (Exception e) {
                log.error("获取登录状态异常，", e);
                deferredResult.setResult(new ResponseEntity<>(ResultUtil.error(ResultCode.ERROR), HttpStatus.OK));
                Thread.currentThread().interrupt();
            }
        }, Executors.newCachedThreadPool());
        return deferredResult;
    }

    @ApiOperation(value = "app扫码")
    @PostMapping(value = "/app_scanner", produces = "application/json;charset=UTF-8")
    public ResultMessage<Object> appScanner(String token) {
        return ResultUtil.data(memberService.appScanner(token));
    }


    @ApiOperation(value = "app扫码-登录确认：同意/拒绝")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "token", value = "sessionToken", required = true, paramType = "query"),
            @ApiImplicitParam(name = "code", value = "操作：0拒绝登录，1同意登录", required = true, paramType = "query")
    })
    @PostMapping(value = "/app_confirm", produces = "application/json;charset=UTF-8")
    public ResultMessage<Object> appSConfirm(String token, Integer code) {
        boolean flag = memberService.appSConfirm(token, code);
        return flag ? ResultUtil.success() : ResultUtil.error(ResultCode.ERROR);
    }


    @ApiOperation(value = "登录接口")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "username", value = "用户名", required = true, paramType = "query"),
            @ApiImplicitParam(name = "password", value = "密码", required = true, paramType = "query")
    })
    @PostMapping("/userLogin")
    public ResultMessage<Object> userLogin(@NotNull(message = "用户名不能为空") @RequestParam String username,
                                           @NotNull(message = "密码不能为空") @RequestParam String password,
                                           @RequestHeader String uuid) {
        verificationService.check(uuid, VerificationEnums.LOGIN);
        return ResultUtil.data(this.memberService.usernameLogin(username, password));
    }

    @ApiOperation(value = "注销接口")
    @PostMapping("/logout")
    public ResultMessage<Object> logout() {
        this.memberService.logout(UserEnums.MEMBER);
        return ResultUtil.success();
    }

    @ApiOperation(value = "短信登录接口")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "mobile", value = "手机号", required = true, paramType = "query"),
            @ApiImplicitParam(name = "code", value = "验证码", required = true, paramType = "query")
    })
    @PostMapping("/smsLogin")
    public ResultMessage<Object> smsLogin(@NotNull(message = "手机号为空") @RequestParam String mobile,
                                          @NotNull(message = "验证码为空") @RequestParam String code,
                                          @RequestHeader String uuid) {
        if (smsUtil.verifyCode(mobile, VerificationEnums.LOGIN, uuid, code)) {
            return ResultUtil.data(memberService.mobilePhoneLogin(mobile));
        } else {
            throw new ServiceException(ResultCode.VERIFICATION_SMS_CHECKED_ERROR);
        }
    }

    @ApiOperation(value = "注册用户")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "username", value = "用户名", required = true, paramType = "query"),
            @ApiImplicitParam(name = "password", value = "密码", required = true, paramType = "query"),
            @ApiImplicitParam(name = "mobilePhone", value = "手机号", required = true, paramType = "query"),
            @ApiImplicitParam(name = "code", value = "验证码", required = true, paramType = "query")
    })
    @PostMapping("/register")
    public ResultMessage<Object> register(@NotNull(message = "用户名不能为空") @RequestParam String username,
                                          @NotNull(message = "密码不能为空") @RequestParam String password,
                                          @NotNull(message = "手机号为空") @RequestParam String mobilePhone,
                                          @RequestHeader String uuid,
                                          @NotNull(message = "验证码不能为空") @RequestParam String code) {

        if (smsUtil.verifyCode(mobilePhone, VerificationEnums.REGISTER, uuid, code)) {
            return ResultUtil.data(memberService.register(username, password, mobilePhone));
        } else {
            throw new ServiceException(ResultCode.VERIFICATION_SMS_CHECKED_ERROR);
        }

    }

    @ApiOperation(value = "获取当前登录用户接口")
    @GetMapping
    public ResultMessage<Member> getUserInfo() {

        return ResultUtil.data(memberService.getUserInfo());
    }

    @ApiOperation(value = "通过短信重置密码")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "mobile", value = "手机号", required = true, paramType = "query"),
            @ApiImplicitParam(name = "password", value = "是否保存登录", required = true, paramType = "query")
    })
    @PostMapping("/resetByMobile")
    public ResultMessage<Member> resetByMobile(@NotNull(message = "手机号为空") @RequestParam String mobile,
                                               @NotNull(message = "验证码为空") @RequestParam String code,
                                               @RequestHeader String uuid) {
        //校验短信验证码是否正确
        if (smsUtil.verifyCode(mobile, VerificationEnums.FIND_USER, uuid, code)) {
            //校验是否通过手机号可获取会员,存在则将会员信息存入缓存，有效时间3分钟
            memberService.findByMobile(uuid, mobile);
            return ResultUtil.success();
        } else {
            throw new ServiceException(ResultCode.VERIFICATION_SMS_CHECKED_ERROR);
        }
    }

    @ApiOperation(value = "修改密码")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "password", value = "是否保存登录", required = true, paramType = "query")
    })
    @PostMapping("/resetPassword")
    public ResultMessage<Object> resetByMobile(@NotNull(message = "密码为空") @RequestParam String password, @RequestHeader String uuid) {

        return ResultUtil.data(memberService.resetByMobile(uuid, password));
    }

    @ApiOperation(value = "修改用户自己资料")
    @PutMapping("/editOwn")
    public ResultMessage<Member> editOwn(MemberEditDTO memberEditDTO) {

        return ResultUtil.data(memberService.editOwn(memberEditDTO));
    }

    @ApiOperation(value = "修改密码")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "password", value = "旧密码", required = true, paramType = "query"),
            @ApiImplicitParam(name = "newPassword", value = "新密码", required = true, paramType = "query")
    })
    @PutMapping("/modifyPass")
    public ResultMessage<Member> modifyPass(@NotNull(message = "旧密码不能为空") @RequestParam String password,
                                            @NotNull(message = "新密码不能为空") @RequestParam String newPassword) {
        return ResultUtil.data(memberService.modifyPass(password, newPassword));
    }

    @ApiOperation(value = "初始设置密码")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "newPassword", value = "新密码", required = true, paramType = "query")
    })
    @PutMapping("/canInitPassword")
    public ResultMessage<Object> canInitPassword() {
        return ResultUtil.data(memberService.canInitPass());
    }

    @ApiOperation(value = "初始设置密码")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "newPassword", value = "新密码", required = true, paramType = "query")
    })
    @PutMapping("/initPassword")
    public ResultMessage<Object> initPassword(@NotNull(message = "密码不能为空") @RequestParam String password) {
        memberService.initPass(password);
        return ResultUtil.success();
    }

    @ApiOperation(value = "注销账号")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "password", value = "密码", required = true, paramType = "query")
    })
    @PutMapping("/cancellation")
    public ResultMessage<Member> cancellation(@NotNull(message = "密码不能为空") @RequestParam String password) {
        memberService.cancellation(password);
        return ResultUtil.success();
    }

    @ApiOperation(value = "刷新token")
    @GetMapping("/refresh/{refreshToken}")
    public ResultMessage<Object> refreshToken(@NotNull(message = "刷新token不能为空") @PathVariable String refreshToken) {
        return ResultUtil.data(this.memberService.refreshToken(refreshToken));
    }

}
