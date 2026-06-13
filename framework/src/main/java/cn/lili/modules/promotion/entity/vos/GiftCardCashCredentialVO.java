package cn.lili.modules.promotion.entity.vos;

import cn.lili.modules.promotion.entity.dos.GiftCardCashCredential;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 卡密凭证分页展示对象（已绑定时带会员摘要信息）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "卡密凭证分页VO（已绑定时带会员摘要）")
public class GiftCardCashCredentialVO extends GiftCardCashCredential {

    @Schema(description = "会员手机号")
    private String memberMobile;

    @Schema(description = "会员昵称")
    private String memberNickName;
}
