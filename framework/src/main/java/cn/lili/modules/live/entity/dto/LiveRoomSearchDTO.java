package cn.lili.modules.live.entity.dto;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.live.entity.dos.LiveRoom;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Data
public class LiveRoomSearchDTO extends PageVO {

    @Schema(title = "直播id")
    private Integer liveId;

    @Schema(title = "直播名称")
    private String liveName;

    @Schema(title = "开始时间")
    private String startTime;

    @Schema(title = "结束时间")
    private String endTime;

    @Schema(title = "直播状态")
    private String liveStatus;

    public QueryWrapper<LiveRoom> getQueryWrapper() {
        QueryWrapper<LiveRoom> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq(liveId != null, "live_id", liveId);
        queryWrapper.like(liveName != null, "live_name", liveName);
        queryWrapper.ge(startTime != null, "start_time", startTime);
        queryWrapper.le(endTime != null, "end_time", endTime);
        queryWrapper.eq(liveStatus != null, "live_status", liveStatus);
        queryWrapper.orderByDesc("start_time");
        return queryWrapper;
    }
}
