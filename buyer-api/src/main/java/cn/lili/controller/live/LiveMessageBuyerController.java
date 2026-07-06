package cn.lili.controller.live;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.live.entity.dos.LiveMessage;
import cn.lili.modules.live.service.LiveMessageService;
import cn.lili.modules.live.service.LiveOperateService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 直播消息接口
 * 
 * @author chc
 * @since 2022/6/21 14:46
 */
@Slf4j
@Validated
@RestController
@Tag(name = "直播消息接口")
@RequestMapping("/buyer/live/message")
@RequiredArgsConstructor
public class LiveMessageBuyerController {

    private final LiveMessageService liveMessageService;

    private final LiveOperateService liveOperateService;

    @Operation(summary = "发送直播消息")
    @PostMapping
    public ResultMessage<LiveMessage> sendMessage(@RequestBody LiveMessage liveMessage) {
        liveOperateService.sendMessage(liveMessage);
        return ResultUtil.data(liveMessage);
    }
}
