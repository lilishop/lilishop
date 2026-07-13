package cn.lili.modules.live.entity.vos;

import cn.lili.modules.live.entity.dos.LiveRoom;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Data
@NoArgsConstructor
@Schema(title = "直播间")
public class LiveRoomVO extends LiveRoom {

    @Schema(title = "互动率")
    private Double interactionRate;

    @Schema(title = "累计销售额")
    private BigDecimal totalAmount;

    @Schema(title = "商品数量")
    private Integer goodsCount;

    @Schema(title = "订单数")
    private Integer orderCount;

    @Schema(title = "下单人数")
    private Integer orderUserCount;

    @Schema(title = "客单价")
    private BigDecimal averagePrice;

    @Schema(title = "转化率")
    private BigDecimal conversionRate;

    /**
     * 密钥Id
     */
    private String secretId;

    /**
     * 直播IM SDK APPID
     */
    private String imSdkAppid;
}
