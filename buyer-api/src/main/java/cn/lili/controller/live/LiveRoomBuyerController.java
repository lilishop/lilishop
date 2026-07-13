package cn.lili.controller.live;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.live.entity.dos.LiveRoom;
import cn.lili.modules.live.entity.dto.LiveRoomSearchDTO;
import cn.lili.modules.live.entity.vos.LiveRoomDetailVO;
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
@RequestMapping("/buyer/live/room")
@RequiredArgsConstructor
public class LiveRoomBuyerController {

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
    public ResultMessage<LiveRoomDetailVO> getById(@NotNull(message = "直播流ID不能为空") @PathVariable String id) {
        return ResultUtil.data(liveRoomService.queryDetail(id));
    }
}
