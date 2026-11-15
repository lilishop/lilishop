package cn.lili.modules.promotion.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 优惠券礼包推广二维码展示VO
 * 包含H5访问链接、标准二维码及微信小程序葵花码；微信生成失败时保留H5结果。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "优惠券礼包推广二维码VO")
public class CouponPackageQrCodeVO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "券礼包活动ID")
    private String activityId;

    @Schema(description = "H5落地页完整访问链接")
    private String h5Url;

    @Schema(description = "二维码PNG图片Data URI，已包含data:image/png;base64,前缀")
    private String qrCodeBase64;

    @Schema(description = "微信小程序葵花码PNG图片Data URI，生成失败时为空；scene通过短链接解析为pack_活动ID")
    private String miniProgramCodeBase64;

    @Schema(description = "微信小程序葵花码生成失败提示，成功时为空，不影响H5二维码使用")
    private String miniProgramCodeError;
}
