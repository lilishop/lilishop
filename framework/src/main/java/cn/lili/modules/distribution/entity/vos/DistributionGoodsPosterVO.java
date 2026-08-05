package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 分销商品海报上下文
 *
 * @author Bulbasaur
 * @since 2026-08-05
 */
@Data
public class DistributionGoodsPosterVO {

    @Schema(description = "小程序葵花码（Base64）")
    private String qrCode;

    @Schema(description = "分享标识（用于访问归因）")
    private String shareId;
}
