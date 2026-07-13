package cn.lili.controller.manager.live;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.live.entity.dos.LiveMessage;
import cn.lili.modules.live.entity.dto.LiveMessageSearchDTO;
import cn.lili.modules.live.service.LiveMessageService;
import cn.lili.modules.live.service.LiveOperateService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.NotNull;
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
@RequestMapping("/manager/live/message")
@RequiredArgsConstructor
public class LiveMessageManagerController {

    private final LiveMessageService liveMessageService;

    private final LiveOperateService liveOperateService;

    @Operation(summary = "分页获取直播消息列表")
    @GetMapping("/page")
    public ResultMessage<IPage<LiveMessage>> getByPage(LiveMessageSearchDTO liveMessageSearchDTO) {
        return ResultUtil.data(liveMessageService.queryMessagePage(liveMessageSearchDTO));
    }

    @Operation(summary = "审核直播消息")
    @PutMapping("/auth")
    public ResultMessage<LiveMessage> authMessage(@RequestBody LiveMessage liveMessage) {
        liveOperateService.authMessage(liveMessage);
        return ResultUtil.data(liveMessage);
    }

    @Operation(summary = "根据用户ID删除直播消息")
    @DeleteMapping("/{liveId}/{userId}")
    public ResultMessage<Object> removeByUserId(
            @NotNull(message = "直播间ID不能为空") @PathVariable String liveId,
            @NotNull(message = "用户ID不能为空") @PathVariable String userId) {
        if (liveMessageService.removeByUserId(liveId, userId)) {
            return ResultUtil.success();
        }
        return ResultUtil.error(ResultCode.LIVE_MESSAGE_REMOVE_EXIST);
    }

    @Operation(summary = "通过id获取直播消息")
    @GetMapping("/{id}")
    public ResultMessage<LiveMessage> getById(@NotNull(message = "消息ID不能为空") @PathVariable String id) {
        return ResultUtil.data(liveMessageService.getById(id));
    }
}
