package cn.lili.modules.live.entity.dto;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.live.entity.dos.LiveMessage;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Data
public class LiveMessageSearchDTO extends PageVO {
    @Schema(title = "直播流ID")
    private String liveStreamId;

    @Schema(title = "直播用户ID")
    private String liveUserId;

    @Schema(title = "用户ID")
    private String userId;

    public QueryWrapper<LiveMessage> getQueryWrapper() {
        QueryWrapper<LiveMessage> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(liveStreamId),"live_room_id", liveStreamId);
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(liveUserId),"live_user_id", liveUserId);
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(userId),"user_id", userId);
        return queryWrapper;
    }
}
