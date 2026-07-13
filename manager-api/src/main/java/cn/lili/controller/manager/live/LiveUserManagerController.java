package cn.lili.controller.manager.live;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.live.entity.dos.LiveUser;
import cn.lili.modules.live.entity.dto.LiveUserSearchDTO;
import cn.lili.modules.live.service.LiveOperateService;
import cn.lili.modules.live.service.LiveUserService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Slf4j
@Validated
@RestController
@Tag(name = "直播用户接口")
@RequestMapping("/manager/live/user")
@RequiredArgsConstructor
public class LiveUserManagerController {

    private final LiveOperateService liveOperateService;

    private final LiveUserService liveUserService;

    @Operation(summary = "直播用户列表")
    @GetMapping(value = "/page")
    public ResultMessage<IPage<LiveUser>> getByPage(LiveUserSearchDTO liveUserSearchDTO) {
        return ResultUtil.data(liveUserService.queryLiveUserList(liveUserSearchDTO));
    }

    @Operation(summary = "更新直播用户禁言状态")
    @PutMapping(value = "/edit/mute")
    public ResultMessage<Object> muteUser(@RequestParam String liveUserid, @RequestParam Boolean muteFlag) {
        liveUserService.editLiveUserMuteFlag(liveUserid, muteFlag);
        return ResultUtil.success();
    }

}
