package cn.lili.controller.manager.live;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.live.entity.dos.LiveRoom;
import cn.lili.modules.live.entity.dto.LiveRoomSearchDTO;
import cn.lili.modules.live.entity.vos.LiveRoomVO;
import cn.lili.modules.live.service.LivePollingDataCacheService;
import cn.lili.modules.live.service.LiveRoomService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 直播流接口
 * 
 * @author chc
 * @since 2022/6/21 14:46
 */
@Slf4j
@Validated
@RestController
@Tag(name = "直播流接口")
@RequestMapping("/manager/live/room")
@RequiredArgsConstructor
public class LiveRoomManagerController {

    private final LiveRoomService liveRoomService;

    private final LivePollingDataCacheService livePollingDataCacheService;

    @Operation(summary = "分页获取直播间列表")
    @GetMapping("/page")
    public ResultMessage<IPage<LiveRoom>> getByPage(LiveRoomSearchDTO liveRoomSearchDTO) {
        return ResultUtil.data(liveRoomService.queryPage(liveRoomSearchDTO));
    }

    @Operation(summary = "获取直播间列表")
    @GetMapping("/list")
    public ResultMessage<List<LiveRoom>> getList(LiveRoomSearchDTO liveRoomSearchDTO) {
        return ResultUtil.data(liveRoomService.queryList(liveRoomSearchDTO));
    }

    @Operation(summary = "通过id获取直播间")
    @GetMapping("/{id}")
    public ResultMessage<LiveRoomVO> getById(@NotNull(message = "直播流ID不能为空") @PathVariable String id) {
        return ResultUtil.data(liveRoomService.getLiveRoomDetailVO(id));
    }

    @Operation(summary = "新增直播间")
    @PostMapping
    public ResultMessage<LiveRoom> save(@RequestBody LiveRoom liveRoom) {
        if (liveRoomService.createLiveRoom(liveRoom)) {
            return ResultUtil.data(liveRoom);
        }
        return ResultUtil.error(ResultCode.LIVE_ROOM_CREATE_ERROR);
    }

    @Operation(summary = "修改直播间")
    @PutMapping("/{id}")
    public ResultMessage<LiveRoom> update(@NotNull(message = "直播流ID不能为空") @PathVariable String id,
                                          @RequestBody LiveRoom liveRoom) {
        liveRoom.setId(id);
        if (liveRoomService.editLiveRoom(liveRoom)) {
            return ResultUtil.data(liveRoom);
        }
        return ResultUtil.error(ResultCode.LIVE_ROOM_EDIT_ERROR);
    }

    @Operation(summary = "开始直播")
    @PutMapping("/start/{id}")
    public ResultMessage<Object> startLiveStream(@NotNull(message = "直播间ID不能为空") @PathVariable String id) {
        liveRoomService.startLiveRoom(id);
        return ResultUtil.success();
    }

    @Operation(summary = "结束直播")
    @PutMapping("/end/{id}")
    public ResultMessage<Object> endLiveStream(@NotNull(message = "直播流ID不能为空") @PathVariable String id) {
        liveRoomService.endLiveRoom(id);
        return ResultUtil.success();
    }

    @Operation(summary = "删除直播流")
    @DeleteMapping("/{id}")
    public ResultMessage<Object> delete(@NotNull(message = "直播流ID不能为空") @PathVariable String id) {
        if (liveRoomService.removeById(id)) {
            return ResultUtil.success();
        }
        return ResultUtil.error(ResultCode.LIVE_ROOM_DELETE_ERROR);
    }
}
