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
@Schema(title = "直播优惠券")
@TableName("li_live_coupon")
public class LiveCoupon extends BaseEntity {
    @Schema(title = "优惠券ID")
    private String couponId;

    @Schema(title = "优惠券金额")
    private Double couponPrice;

    @Schema(title = "优惠券名称")
    private String couponName;

    @Schema(title = "直播间ID")
    private String liveRoomId;

    @Schema(title = "直播间名称")
    private String liveRoomName;

    @Schema(title = "是否隐藏")
    private Boolean hideFlag;

    @Schema(title = "是否推荐")
    private Boolean recommend;
}
