package cn.lili.modules.live.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Data
@NoArgsConstructor
@Schema(title = "直播黑名单")
@TableName("li_live_block")
public class LiveBlock  extends BaseEntity {

    @Schema(title = "用户ID")
    private String userId;

    @Schema(title = "用户名")
    private String userName;

    @Schema(title = "用户头像")
    private String userAvatar;

    @Schema(title = "直播间ID")
    private String liveRoomId;

    @Schema(title = "店铺ID")
    private String storeId;

    @Schema(title = "店铺ID")
    private String storeName;

    @Schema(title = "拉黑原因")
    private String reason;
}
