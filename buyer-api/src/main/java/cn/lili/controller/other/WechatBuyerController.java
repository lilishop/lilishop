package cn.lili.controller.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.wechat.util.WechatUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@Tag(name = "买家端,微信公众号接口")
@RequestMapping("/buyer/other/wechat")
public class WechatBuyerController {

    @Autowired
    private WechatUtil wechatUtil;

    @GetMapping("/h5/qrcode")
    @Operation(summary = "获取微信公众号二维码(base64)")
    @Parameter(name = "scene", description = "二维码场景值(scene_str)，建议使用业务唯一值，长度不超过64")
    @Parameter(name = "expireSeconds", description = "过期时间(秒)，最大2592000；不传默认2592000")
    public ResultMessage<String> h5QrCode(@RequestParam(required = false) String scene,
                                         @RequestParam(required = false) Integer expireSeconds) {
        return ResultUtil.data(wechatUtil.h5QrCodeBase64(scene, expireSeconds));
    }

    @GetMapping(value = "/h5/callback", produces = "text/plain;charset=UTF-8")
    @Operation(summary = "微信公众号回调校验")
    public String h5CallbackCheck(@RequestParam(required = false) String signature,
                                  @RequestParam(required = false) String timestamp,
                                  @RequestParam(required = false) String nonce,
                                  @RequestParam(required = false) String echostr) {
        if (wechatUtil.checkSignature(signature, timestamp, nonce)) {
            return echostr;
        }
        return "invalid signature";
    }

    @PostMapping(value = "/h5/callback", produces = "application/xml;charset=UTF-8")
    @Operation(summary = "微信公众号回调接收")
    public String h5Callback(@RequestBody(required = false) String xml) {
        return wechatUtil.handleH5Callback(xml);
    }
}
