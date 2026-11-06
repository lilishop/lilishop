package cn.lili.modules.promotion.entity.vos;

import cn.lili.modules.promotion.entity.dos.GiftCardCashActivity;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.List;

/**
 * 礼品卡活动展示对象，供列表和详情返回库存统计，详情额外提供提货商品的实时回显数据。
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "礼品卡活动展示数据")
public class GiftCardCashActivityVO extends GiftCardCashActivity {

    @Schema(description = "制卡数（张），即活动下未发放凭据池数量，可导出卡密或由用户自助绑定；从凭据记录统计")
    private Integer producedQuantity;

    @Schema(description = "发卡数（张），即通过导入直接发放给会员的凭据数量；根据发卡批次关联统计")
    private Integer issuedCardQuantity;

    @Schema(description = "剩余可制发库存（张），等于总库存减去活动下未逻辑删除的凭据总数；制卡与发卡共用")
    private Integer remainingStock;

    @Schema(description = "提货卡候选商品实时展示数据，供管理端详情回显；不包含卡密或内部成本字段")
    private List<PickupCardSkuVO> pickupGoods;
}
