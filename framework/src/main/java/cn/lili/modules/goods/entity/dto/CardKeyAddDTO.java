package cn.lili.modules.goods.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.io.Serializable;

/**
 * 单条新增卡密
 *
 * @author Mike
 * @date 2026-07-31
 */
@Data
@Schema(description = "单条新增卡密")
public class CardKeyAddDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @NotBlank(message = "SKU ID不能为空")
    @Schema(description = "SKU ID", requiredMode = Schema.RequiredMode.REQUIRED)
    private String skuId;

    @NotBlank(message = "卡号不能为空")
    @Schema(description = "卡号", requiredMode = Schema.RequiredMode.REQUIRED)
    private String cardNo;

    @NotBlank(message = "卡密不能为空")
    @Schema(description = "卡密", requiredMode = Schema.RequiredMode.REQUIRED)
    private String cardSecret;
}
