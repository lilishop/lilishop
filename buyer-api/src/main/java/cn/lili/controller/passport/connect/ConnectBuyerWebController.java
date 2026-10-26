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
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * 买家端,web联合登录
 *
 * @author Chopper
 */
@Slf4j
@RestController
@Tag(name = "买家端,web联合登录")
@RequestMapping("/buyer/passport/connect/connect")
public class ConnectBuyerWebController {

    @Autowired
    private ConnectService connectService;

    @Autowired
    private ConnectUtil connectUtil;


    @GetMapping("/login/web/{type}")
    @Operation(summary = "WEB信任登录授权,包含PC、WAP")
    @Parameters({
            @Parameter(name = "type", description = "登录方式:QQ,微信,微信_PC")
    })
    public ResultMessage<String> webAuthorize(@PathVariable String type, HttpServletResponse response) throws IOException {
        AuthRequest authRequest = connectUtil.getAuthRequest(type);
        String authorizeUrl = authRequest.authorize(UuidUtils.getUUID());
        response.sendRedirect(authorizeUrl);
        return ResultUtil.data(authorizeUrl);
    }


    @Operation(summary = "信任登录统一回调地址", hidden = true)
    @GetMapping("/callback/{type}")
    public void callBack(@PathVariable String type, AuthCallback callback, HttpServletRequest httpServletRequest, HttpServletResponse httpServletResponse) throws IOException {
        connectUtil.callback(type, callback, httpServletRequest, httpServletResponse);
    }

    @Operation(summary = "信任登录响应结果获取")
    @GetMapping("/result")
    public ResultMessage<Object> callBackResult(String state) {
        if (state == null) {
            throw new ServiceException(ResultCode.USER_CONNECT_LOGIN_ERROR);
        }
        return connectUtil.getResult(state);
    }

    @Operation(summary = "APP-unionID登录")
    @PostMapping("/app/login")
    public ResultMessage<Token> unionLogin(@RequestBody ConnectAuthUser authUser, @RequestHeader("uuid") String uuid) {
        try {
            return ResultUtil.data(connectService.unionLoginCallback(authUser, uuid));
        } catch (Exception e) {
            log.error("unionID登录错误", e);
        }
        return null;
    }

}
