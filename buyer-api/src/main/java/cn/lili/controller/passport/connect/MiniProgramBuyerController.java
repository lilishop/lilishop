package cn.lili.controller.passport.connect;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.token.Token;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.connect.entity.dto.WechatMPLoginParams;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.message.service.ShortLinkService;
import cn.lili.modules.wechat.entity.dos.WechatMPMessage;
import cn.lili.modules.wechat.service.WechatMPMessageService;
import cn.lili.modules.wechat.util.WechatMpCodeUtil;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 买家端,小程序登录接口
 *
 * @author Chopper
 * @since 2021/2/19 09:28
 */
@RestController
@RequestMapping("/buyer/mini-program")
@Api(tags = "买家端,小程序登录接口")
public class MiniProgramBuyerController {

    @Autowired
    public ConnectService connectService;
    @Autowired
    public WechatMpCodeUtil wechatMpCodeUtil;
    @SuppressWarnings("AlibabaLowerCamelCaseVariableNaming")
    @Autowired
    public WechatMPMessageService wechatMPMessageService;
    @Autowired
    public ShortLinkService shortLinkService;

    @GetMapping("/auto-login")
    @ApiOperation(value = "小程序自动登录")
    public ResultMessage<Token> autoLogin(@RequestHeader String uuid, WechatMPLoginParams params) {
        params.setUuid(uuid);
        return ResultUtil.data(this.connectService.miniProgramAutoLogin(params));
    }

    @GetMapping("/subscribeMessage")
    @ApiOperation(value = "消息订阅")
    public ResultMessage<List<WechatMPMessage>> autoLogin() {
        return ResultUtil.data(wechatMPMessageService.list());
    }

    @GetMapping("/mp/unlimited")
    @ApiOperation(value = "小程序二维码生成：不限制数量，但是限制长度，只能存放32为长度")
    public ResultMessage<String> unlimited(String page, String scene) {
        return ResultUtil.data(wechatMpCodeUtil.createCode(page, scene));
    }

    @GetMapping("/mp/qrcode")
    @ApiOperation(value = "小程序二维码生成:只适用于少量场景，多数场景需要unlimited接口实现")
    public ResultMessage<String> qrcode(String page) {
        return ResultUtil.data(wechatMpCodeUtil.createQrCode(page));
    }

    @GetMapping("/mp/unlimited/scene")
    @ApiOperation(value = "根据shortlink 获取页面参数")
    public ResultMessage<String> getScene(String id) {
        return ResultUtil.data(shortLinkService.getById(id).getOriginalParams());
    }

}
