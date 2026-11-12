package cn.lili.modules.promotion.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 优惠券礼包推广二维码展示VO
 * 包含H5独立页面访问链接以及生成的标准二维码Base64图片数据
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

    @Schema(description = "二维码Base64图片数据")
    private String qrCodeBase64;
}
