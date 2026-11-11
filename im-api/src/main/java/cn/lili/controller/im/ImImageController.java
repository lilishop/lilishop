package cn.lili.controller.im;

import cn.lili.cache.Cache;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.im.service.ImImageService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/**
 * 客服会话图片上传入口，显式校验 accessToken，业务权限与文件校验统一委托服务层。
 * @author Bulbasaur
 * @since 2026-11-09
 */
@RestController
@RequestMapping("/im/message/image")
@Tag(name = "客服图片")
@RequiredArgsConstructor
public class ImImageController {
    private final Cache cache;
    private final ImImageService imImageService;

    /**
     * 上传图片到客服独立桶，店铺与路径从会话推导，接口不接受客户端存储参数。
     * @param accessToken 当前登录凭据
     * @param talkId li_im_talk.id
     * @param file 待发送图片
     * @return 公共读图片永久地址
     */
    @PostMapping
    @Operation(summary = "上传客服会话图片")
    public ResultMessage<String> upload(@RequestHeader String accessToken, @RequestParam String talkId,
                                        @RequestParam MultipartFile file) {
        return ResultUtil.data(imImageService.upload(UserContext.getAuthUser(cache, accessToken), talkId, file));
    }
}
