package cn.lili.controller.passport.connect;


import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.token.Token;
import cn.lili.common.utils.UuidUtils;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.connect.entity.dto.AuthCallback;
import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.connect.request.AuthRequest;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.connect.util.ConnectUtil;
import cn.lili.modules.member.service.MemberService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * 买家端,web联合登录
 *
 * @author Chopper
 */
@Slf4j
@RestController
@Api(tags = "买家端,web联合登录")
@RequestMapping("/buyer/connect")
public class ConnectBuyerWebController {

    @Autowired
    private ConnectService connectService;

    @Autowired
    private MemberService memberService;

    @Autowired
    private ConnectUtil connectUtil;


    @GetMapping("/login/web/{type}")
    @ApiOperation(value = "WEB信任登录授权")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "登录方式:QQ,微信,微信_PC",
                    allowableValues = "QQ,WECHAT,WECHAT_PC", paramType = "path")
    })
    public ResultMessage<String> webAuthorize(@PathVariable String type, HttpServletResponse response) throws IOException {
        AuthRequest authRequest = connectUtil.getAuthRequest(type);
        String authorizeUrl = authRequest.authorize(UuidUtils.getUUID());
        response.sendRedirect(authorizeUrl);
        return ResultUtil.data(authorizeUrl);
    }


    @ApiOperation(value = "信任登录统一回调地址", hidden = true)
    @GetMapping("/callback/{type}")
    public void callBack(@PathVariable String type, AuthCallback callback, HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) throws IOException {
        connectUtil.callback(type, callback, httpServletRequest, httpServletResponse);
    }

    @ApiOperation(value = "信任登录响应结果获取")
    @GetMapping("/result")
    public ResultMessage<Object> callBackResult(String state) {
        if (state == null) {
            throw new ServiceException(ResultCode.USER_CONNECT_LOGIN_ERROR);
        }
        return connectUtil.getResult(state);
    }

    @GetMapping("/register/auto")
    @ApiOperation(value = "WEB信任登录授权")
    public ResultMessage<Token> webAuthorize() {
        Token token = memberService.autoRegister();
        return ResultUtil.data(token);
    }

    @ApiOperation(value = "unionID登录")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "openId", value = "openid", required = true, paramType = "query"),
            @ApiImplicitParam(name = "type", value = "联合类型", required = true,
                    allowableValues = "WECHAT,QQ,ALIPAY,WEIBO,APPLE", paramType = "query"),
            @ApiImplicitParam(name = "uniAccessToken", value = "联合登陆返回的accessToken", required = true, paramType = "query")
    })
    @GetMapping("/app/login")
    public ResultMessage<Token> unionLogin(ConnectAuthUser authUser, @RequestHeader("uuid") String uuid) {
        try {
            return ResultUtil.data(connectService.appLoginCallback(authUser, uuid));
        } catch (Exception e) {
            log.error("unionID登录错误", e);
        }
        return null;
    }

}
