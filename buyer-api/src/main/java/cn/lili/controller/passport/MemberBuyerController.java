package cn.lili.controller.passport;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.MemberEditDTO;
import cn.lili.modules.member.entity.enums.QRCodeLoginSessionStatusEnum;
import cn.lili.modules.member.entity.vo.ImUserDetailVO;
import cn.lili.modules.member.entity.vo.QRLoginResultVo;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.member.service.MemberShareRegisterService;
import cn.lili.modules.member.service.MemberShareLogService;
import cn.lili.modules.sms.SmsUtil;
import cn.lili.modules.verification.entity.enums.VerificationEnums;
import cn.lili.modules.verification.service.VerificationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.async.DeferredResult;

import jakarta.validation.constraints.NotNull;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * 买家端,客户接口
 *
 * @author Chopper
 * @since 2020/11/16 10:07 下午
 */
@Slf4j
@RestController
@Tag(name = "买家端,客户接口")
@RequestMapping("/buyer/passport/member")
public class MemberBuyerController {

    @Autowired
    private MemberService memberService;
    @Autowired
    private SmsUtil smsUtil;
    @Autowired
    private VerificationService verificationService;
    @Autowired
    private MemberShareLogService memberShareLogService;
    @Autowired
    private MemberShareRegisterService memberShareRegisterService;


    @Operation(summary = "web-获取登录二维码")
    @PostMapping("/pc_session")
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
    @Operation(summary = "web-二维码登录")
    @PostMapping("/session_login/{token}")
    public Object loginWithSession(@PathVariable("token") String token, Integer beforeSessionStatus) {
        log.info("receive login with session key {}", token);
        ResponseEntity<ResultMessage<Object>> timeoutResponseEntity =
                new ResponseEntity<>(ResultUtil.error(ResultCode.ERROR), HttpStatus.OK);
        int timeoutSecond = 20;
        DeferredResult<ResponseEntity<Object>> deferredResult = new DeferredResult<>(timeoutSecond * 1000L, timeoutResponseEntity);
         // 用于记录重试次数
        AtomicInteger retryCount = new AtomicInteger(0);
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
                        //TimeUnit.SECONDS.sleep(1);
                        
                        // 应用指数退避策略
                        int baseSleepTime = 1000;  // 基础退避时间（毫秒）
                        int maxSleepTime = 10000;  // 最大退避时间（毫秒）

                        int sleepTime = Math.min(maxSleepTime, baseSleepTime * (1 + retryCount.getAndIncrement()));
                        int randomFactor = (int) (Math.random() * (sleepTime / 2));  // 随机化因子

                        TimeUnit.MILLISECONDS.sleep(sleepTime + randomFactor);
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
        });
        return deferredResult;
    }

    @Operation(summary = "app扫码")
    @PostMapping("/app_scanner")
    public ResultMessage<Object> appScanner(String token) {
        return ResultUtil.data(memberService.appScanner(token));
    }


    @Operation(summary = "app扫码-登录确认：同意/拒绝")
    @Parameters({
            @Parameter(name = "token", description = "sessionToken", required = true),
            @Parameter(name = "code", description = "操作：0拒绝登录，1同意登录", required = true)
    })
    @PostMapping("/app_confirm")
    public ResultMessage<Object> appSConfirm(String token, Integer code) {
        boolean flag = memberService.appSConfirm(token, code);
        return flag ? ResultUtil.success() : ResultUtil.error(ResultCode.ERROR);
    }


    @Operation(summary = "登录接口")
    @Parameters({
            @Parameter(name = "username", description = "用户名", required = true),
            @Parameter(name = "password", description = "密码", required = true)
    })
    @PostMapping("/userLogin")
    public ResultMessage<Object> userLogin(@NotNull(message = "用户名不能为空") @RequestParam String username,
                                           @NotNull(message = "密码不能为空") @RequestParam String password,
                                           @RequestHeader String uuid) {
        verificationService.check(uuid, VerificationEnums.LOGIN);
        return ResultUtil.data(this.memberService.usernameLogin(username, password));
    }

    @Operation(summary = "注销接口")
    @PostMapping("/logout")
    public ResultMessage<Object> logout() {
        this.memberService.logout(UserEnums.MEMBER);
        return ResultUtil.success();
    }

    @Operation(summary = "短信登录接口")
    @Parameters({
            @Parameter(name = "mobile", description = "手机号", required = true),
            @Parameter(name = "code", description = "验证码", required = true)
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

    @Operation(summary = "绑定手机号")
    @Parameters({
            @Parameter(name = "username", description = "用户名", required = true),
            @Parameter(name = "mobile", description = "手机号", required = true),
            @Parameter(name = "code", description = "验证码", required = true)
    })
    @PostMapping("/bindMobile")
    public ResultMessage<Object> bindMobile(@NotNull(message = "用户名不能为空") @RequestParam String username,
                                            @NotNull(message = "手机号为空") @RequestParam String mobile,
                                            @NotNull(message = "验证码为空") @RequestParam String code,
                                            @RequestHeader String uuid) {
        if (smsUtil.verifyCode(mobile, VerificationEnums.BIND_MOBILE, uuid, code)) {
            Member member = memberService.findByUsername(username);
            Member memberByMobile = memberService.findByMobile(mobile);
            if (member == null) {
                throw new ServiceException(ResultCode.USER_NOT_EXIST);
            }
            if(memberByMobile != null){
                throw new ServiceException(ResultCode.USER_MOBILE_REPEATABLE_ERROR);
            }
            return ResultUtil.data(memberService.changeMobile(member.getId(), mobile));
        } else {
            throw new ServiceException(ResultCode.VERIFICATION_SMS_CHECKED_ERROR);
        }
    }

    @Operation(summary = "注册用户")
    @Parameters({
            @Parameter(name = "username", description = "用户名", required = true),
            @Parameter(name = "password", description = "密码", required = true),
            @Parameter(name = "mobilePhone", description = "手机号", required = true),
            @Parameter(name = "shareCode", description = "分享码", required = false)
    })
    @PostMapping("/register")
    public ResultMessage<Object> register(@NotNull(message = "用户名不能为空") @RequestParam String username,
                                          @NotNull(message = "密码不能为空") @RequestParam String password,
                                          @NotNull(message = "手机号为空") @RequestParam String mobilePhone,
                                          @RequestHeader String uuid,
                                          @NotNull(message = "验证码不能为空") @RequestParam String code,
                                          @RequestParam(required = false) String shareCode) {

        if (smsUtil.verifyCode(mobilePhone, VerificationEnums.REGISTER, uuid, code)) {
            return ResultUtil.data(memberService.register(username, password, mobilePhone, shareCode));
        } else {
            throw new ServiceException(ResultCode.VERIFICATION_SMS_CHECKED_ERROR);
        }

    }

   

    @Operation(summary = "获取当前登录用户接口")
    @GetMapping
    public ResultMessage<Member> getUserInfo() {

        return ResultUtil.data(memberService.getUserInfo());
    }

    @Operation(summary = "通过短信重置密码")
    @Parameters({
            @Parameter(name = "mobile", description = "手机号", required = true),
            @Parameter(name = "password", description = "是否保存登录", required = true)
    })
    @PostMapping("/resetByMobile")
    public ResultMessage<Member> resetByMobile(@NotNull(message = "手机号为空") @RequestParam String mobile,
                                               @NotNull(message = "验证码为空") @RequestParam String code,
                                               @RequestHeader String uuid) {
        //校验短信验证码是否正确
        if (smsUtil.verifyCode(mobile, VerificationEnums.FIND_USER, uuid, code)) {
            //校验是否通过手机号可获取客户,存在则将客户信息存入缓存，有效时间3分钟
            memberService.findByMobile(uuid, mobile);
            return ResultUtil.success();
        } else {
            throw new ServiceException(ResultCode.VERIFICATION_SMS_CHECKED_ERROR);
        }
    }

    @Operation(summary = "修改密码")
    @Parameters({
            @Parameter(name = "password", description = "是否保存登录", required = true)
    })
    @PostMapping("/resetPassword")
    public ResultMessage<Object> resetByMobile(@NotNull(message = "密码为空") @RequestParam String password, @RequestHeader String uuid) {

        return ResultUtil.data(memberService.resetByMobile(uuid, password));
    }

    @Operation(summary = "修改用户自己资料")
    @PutMapping("/editOwn")
    public ResultMessage<Member> editOwn(MemberEditDTO memberEditDTO) {

        return ResultUtil.data(memberService.editOwn(memberEditDTO));
    }

    @Operation(summary = "修改密码")
    @Parameters({
            @Parameter(name = "password", description = "旧密码", required = true),
            @Parameter(name = "newPassword", description = "新密码", required = true)
    })
    @PutMapping("/modifyPass")
    public ResultMessage<Member> modifyPass(@NotNull(message = "旧密码不能为空") @RequestParam String password,
                                            @NotNull(message = "新密码不能为空") @RequestParam String newPassword) {
        return ResultUtil.data(memberService.modifyPass(password, newPassword));
    }

    @Operation(summary = "初始设置密码")
    @Parameters({
            @Parameter(name = "newPassword", description = "新密码", required = true)
    })
    @PutMapping("/canInitPassword")
    public ResultMessage<Object> canInitPassword() {
        return ResultUtil.data(memberService.canInitPass());
    }

    @Operation(summary = "初始设置密码")
    @Parameters({
            @Parameter(name = "newPassword", description = "新密码", required = true)
    })
    @PutMapping("/initPassword")
    public ResultMessage<Object> initPassword(@NotNull(message = "密码不能为空") @RequestParam String password) {
        memberService.initPass(password);
        return ResultUtil.success();
    }

    @Operation(summary = "注销账号")
    @PutMapping("/cancellation")
    public ResultMessage<Member> cancellation() {
        memberService.cancellation();
        return ResultUtil.success();
    }

    @Operation(summary = "刷新token")
    @GetMapping("/refresh/{refreshToken}")
    public ResultMessage<Object> refreshToken(@NotNull(message = "刷新token不能为空") @PathVariable String refreshToken) {
        return ResultUtil.data(this.memberService.refreshToken(refreshToken));
    }

    @GetMapping("/getImUser")
    @Operation(summary = "获取用户信息")
    public ResultMessage<Member> getImUser() {
        AuthUser authUser = UserContext.getCurrentUser();
        return ResultUtil.data(memberService.getById(authUser.getId()));
    }

    @GetMapping("/getImUserDetail/{memberId}")
    @Parameter(name = "memberId", description = "会员Id", required = true)
    @Operation(summary = "获取IM用户信息（仅昵称/头像等公开字段）")
    public ResultMessage<ImUserDetailVO> getImUserDetail(@PathVariable String memberId) {
        Member member = memberService.getById(memberId);
        if (member == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        return ResultUtil.data(new ImUserDetailVO(member));
    }

    @Operation(summary = "分享商城页面赠送经验值")
    @PostMapping("/share/mall")
    public ResultMessage<Object> shareMall() {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        memberShareLogService.saveShareAndGrantExperience(currentUser.getId(), "MALL", "MALL_HOME", null);
        return ResultUtil.success();
    }

    @Operation(summary = "获取分享注册码")
    @GetMapping("/share/register/code")
    public ResultMessage<String> getRegisterShareCode() {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        return ResultUtil.data(memberShareRegisterService.getOrCreateRegisterShareCode(currentUser.getId()));
    }

}
