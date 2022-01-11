package cn.lili.controller.common;

import cn.lili.cache.limit.annotation.LimitPoint;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.verification.entity.enums.VerificationEnums;
import cn.lili.modules.verification.service.VerificationService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 滑块验证码接口
 *
 * @author Chopper
 * @since 2020/11/26 15:41
 */
@Slf4j
@RestController
@RequestMapping("/common/common/slider")
@Api(tags = "滑块验证码接口")
public class SliderImageController {

    @Autowired
    private VerificationService verificationService;

    @LimitPoint(name = "slider_image", key = "verification")
    @GetMapping("/{verificationEnums}")
    @ApiOperation(value = "获取校验接口,一分钟同一个ip请求10次")
    public ResultMessage getSliderImage(@RequestHeader String uuid, @PathVariable VerificationEnums verificationEnums) {
        return ResultUtil.data(verificationService.createVerification(verificationEnums, uuid));

    }

    @LimitPoint(name = "slider_image", key = "verification_pre_check", limit = 600)
    @PostMapping("/{verificationEnums}")
    @ApiOperation(value = "验证码预校验")
    public ResultMessage verificationImage(Integer xPos, @RequestHeader String uuid, @PathVariable VerificationEnums verificationEnums) {
        return ResultUtil.data(verificationService.preCheck(xPos, uuid, verificationEnums));
    }
}
