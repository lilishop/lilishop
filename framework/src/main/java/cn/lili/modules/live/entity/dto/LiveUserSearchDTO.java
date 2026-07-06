package cn.lili.modules.live.entity.dto;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.live.entity.dos.LiveUser;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Data
public class LiveUserSearchDTO extends PageVO {
    @Schema(title = "直播间ID")
    private String liveRoomId;

    @Schema(title = "用户ID")
    private String userId;

    @Schema(title = "禁言标识")
    private Boolean muteFlag;

    public LambdaQueryWrapper<LiveUser> getQueryWrapper() {
        LambdaQueryWrapper<LiveUser> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(liveRoomId), LiveUser::getLiveRoomId, liveRoomId);
        queryWrapper.eq(CharSequenceUtil.isNotEmpty(userId), LiveUser::getUserId, userId);
        if(muteFlag != null){
            queryWrapper.eq(LiveUser::getMuteFlag, muteFlag);
        }
        return queryWrapper;
    }
}
