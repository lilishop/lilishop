package cn.lili.controller.common;

import cn.lili.cache.limit.annotation.LimitPoint;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.sms.SmsUtil;
import cn.lili.modules.verification.entity.enums.VerificationEnums;
import cn.lili.modules.verification.service.VerificationService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 短信验证码接口
 *
 * @author Chopper
 * @since 2020/11/26 15:41
 */
@RestController
@Api(tags = "短信验证码接口")
@RequestMapping("/common/common/sms")
public class SmsController {

    @Autowired
    private SmsUtil smsUtil;
    @Autowired
    private VerificationService verificationService;

    @LimitPoint(name = "sms_send", key = "sms")
    @ApiImplicitParams({
            @ApiImplicitParam(paramType = "path", dataType = "String", name = "mobile", value = "手机号"),
            @ApiImplicitParam(paramType = "header", dataType = "String", name = "uuid", value = "uuid"),
    })
    @GetMapping("/{verificationEnums}/{mobile}")
    @ApiOperation(value = "发送短信验证码,一分钟同一个ip请求1次")
    public ResultMessage getSmsCode(
            @RequestHeader String uuid,
            @PathVariable String mobile,
            @PathVariable VerificationEnums verificationEnums) {
        verificationService.check(uuid, verificationEnums);
        smsUtil.sendSmsCode(mobile, verificationEnums, uuid);
        return ResultUtil.success(ResultCode.VERIFICATION_SEND_SUCCESS);
    }
}
