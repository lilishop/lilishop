package cn.lili.controller.manager.live;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.live.entity.dos.LiveBlock;
import cn.lili.modules.live.service.LiveBlockService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Slf4j
@Validated
@RestController
@Tag(name = "直播拉黑接口")
@RequestMapping("/manager/live/block")
@RequiredArgsConstructor
public class LiveBlockManagerController {

    private final LiveBlockService liveBlockService;

    /**
     * 根据直播间ID分页查询拉黑用户列表
     *
     * @param page 分页信息
     * @return 拉黑用户列表
     */
    @PostMapping("/page")
    public IPage<LiveBlock> liveBlockPage(PageVO page) {
        return liveBlockService.liveBlockPage(page);
    }

    /**
     * 拉黑用户
     *
     * @param userId    用户id
     * @param liveRoomId 直播房间id
     * @param reason    拉黑原因
     */
    @PostMapping("/user")
    public void blockUser(@RequestParam String userId, @RequestParam String liveRoomId, @RequestParam String reason) {
        liveBlockService.blockUser(userId, liveRoomId, reason);
    }

     /**
     * 取消拉黑用户
     *
     * @param userId    用户id
     * @param liveRoomId 直播房间id
     */
    @PostMapping("/user/unblock")
    public void unblockUser(@RequestParam String userId, @RequestParam String liveRoomId) {
        liveBlockService.unblockUser(userId, liveRoomId);
    }
}
