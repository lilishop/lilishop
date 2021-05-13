package cn.lili.controller.common;

import cn.lili.common.aop.limiter.annotation.LimitPoint;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.ResultUtil;
import cn.lili.common.verification.enums.VerificationEnums;
import cn.lili.common.verification.service.VerificationService;
import cn.lili.common.vo.ResultMessage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 滑块验证码接口
 *
 * @author Chopper
 * @date 2020/11/26 15:41
 */
@RequestMapping("/common/slider")
@RestController
@Api(tags = "滑块验证码接口")
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class SliderImageController {


    private final VerificationService verificationService;

    //一分钟同一个ip请求10次
    @LimitPoint(name = "slider_image", key = "verification")
    @GetMapping("/{verificationEnums}")
    @ApiOperation(value = "获取校验接口")
    public ResultMessage getSliderImage(@RequestHeader String uuid, @PathVariable VerificationEnums verificationEnums) {
        try {
            return ResultUtil.data(verificationService.createVerification(verificationEnums, uuid));
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    @LimitPoint(name = "slider_image", key = "verification_pre_check", limit = 600)
    @PostMapping("/{verificationEnums}")
    @ApiOperation(value = "验证码预校验")
    public ResultMessage verificationImage(Integer xPos, @RequestHeader String uuid, @PathVariable VerificationEnums verificationEnums) {
        return ResultUtil.data(verificationService.preCheck(xPos, uuid, verificationEnums));
    }
}
