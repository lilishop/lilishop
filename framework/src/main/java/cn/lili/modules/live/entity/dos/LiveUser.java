package cn.lili.modules.live.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@Schema(title = "直播间用户")
@TableName("li_live_user")
public class LiveUser extends BaseEntity {
    @Schema(title = "直播流ID")
    private String liveRoomId;

    @Schema(title = "用户ID")
    private String userId;

    @Schema(title = "用户名")
    private String userName;

    @Schema(title = "用户头像")
    private String userFace;

    @Schema(title = "观看时间")
    private String watchTime;

    @Schema(title = "禁言标识")
    private Boolean muteFlag;

    @Schema(title = "用户腾讯云Sig")
    private String userSig;

    @Schema(title = "消费金额")
    private Double amount;

}
