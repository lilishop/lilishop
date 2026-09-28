package cn.lili.modules.order.order.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.Data;

import java.io.Serializable;

/**
 * 按核销码核销
 *
 * @author Bulbasaur
 * @since 2026-09-23
 */
@Data
@Schema(description = "按核销码核销")
public class OrderVerificationConfirmDTO implements Serializable {

    @NotBlank(message = "核销码不能为空")
    @Schema(description = "核销码")
    private String code;
}
