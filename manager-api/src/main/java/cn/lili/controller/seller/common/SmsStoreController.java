package cn.lili.controller.seller.common;

import cn.lili.cache.limit.annotation.LimitPoint;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.sms.SmsUtil;
import cn.lili.modules.verification.entity.enums.VerificationEnums;
import cn.lili.modules.verification.service.VerificationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 短信验证码接口
 *
 * @author Chopper
 * @since 2020/11/26 15:41
 */
@RestController
@Tag(name = "短信验证码接口")
@RequestMapping("/store/common/sms")
public class SmsStoreController {

    @Autowired
    private SmsUtil smsUtil;
    @Autowired
    private VerificationService verificationService;

    @LimitPoint(name = "sms_send", key = "sms")
    @Parameters({
            @Parameter(name = "mobile", description = "手机号"),
            @Parameter(name = "uuid", description = "uuid"),
    })
    @GetMapping("/{verificationEnums}/{mobile}")
    @Operation(summary = "发送短信验证码,一分钟同一个ip请求1次")
    public ResultMessage getSmsCode(
            @RequestHeader String uuid,
            @PathVariable String mobile,
            @PathVariable VerificationEnums verificationEnums) {
        verificationService.check(uuid, verificationEnums);
        smsUtil.sendSmsCode(mobile, verificationEnums, uuid);
        return ResultUtil.success(ResultCode.VERIFICATION_SEND_SUCCESS);
    }
}
