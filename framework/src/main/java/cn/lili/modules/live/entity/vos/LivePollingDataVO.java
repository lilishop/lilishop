package cn.lili.modules.live.entity.vos;

import cn.lili.modules.live.entity.dos.LiveCoupon;
import cn.lili.modules.live.entity.dos.LiveGoods;
import cn.lili.modules.live.entity.dos.LiveRoom;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(title = "直播聚合")
public class LivePollingDataVO {

    private static final long serialVersionUID = 1L;

    @Schema(title = "直播详情")
    private LiveRoom liveDetail;

    @Schema(title = "直播商品列表")
    private List<LiveGoods> goodsList;

    @Schema(title = "直播优惠券配置")
    private List<LiveCoupon> liveCoupon;

    @Schema(title = "当前用户直播优惠券领取记录")
    private List<MemberCoupon> liveCouponReceives;
}
