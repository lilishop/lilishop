package cn.lili.modules.live.entity.dos;

import cn.lili.modules.live.entity.enums.LiveMessageAuthStatusEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@Schema(title = "直播间消息")
@TableName("li_live_message")
public class LiveMessage extends BaseEntity {
    @Schema(title = "直播间ID")
    private String liveRoomId;

    @Schema(title = "直播名称")
    private String liveName;

    @Schema(title = "直播用户ID")
    private String liveUserId;

    @Schema(title = "用户ID")
    private String userId;

    @Schema(title = "用户名")
    private String userName;

    @Schema(title = "用户头像")
    private String userFace;

    @Schema(title = "消息内容")
    private String message;

    @Schema(title = "IM群ID")
    private String imGroupId;

    /**
     * @see LiveMessageAuthStatusEnum
     */
    @Schema(title = "审核状态")
    private String authStatus;
}
