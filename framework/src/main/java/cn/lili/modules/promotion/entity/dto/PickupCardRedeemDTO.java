package cn.lili.modules.promotion.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Size;
import lombok.Data;

import java.util.List;

/**
 * 提货卡兑换请求，仅接受卡、SKU 和地址标识，价格及权益以服务端为准。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
@Data
@Schema(description = "提货卡兑换请求")
public class PickupCardRedeemDTO {

    @NotBlank
    @Schema(description = "提货卡凭证 ID，对应 li_gift_card_cash_credential.id")
    private String credentialId;

    @NotEmpty
    @Size(max = 50)
    @Schema(description = "本次兑换的不同 SKU ID 列表，对应 li_goods_sku.id；1～50 个，每种一件，种数须符合卡规则")
    private List<@NotBlank String> skuIds;

    @NotBlank
    @Schema(description = "当前会员收货地址 ID，对应 li_member_address.id")
    private String addressId;
}
