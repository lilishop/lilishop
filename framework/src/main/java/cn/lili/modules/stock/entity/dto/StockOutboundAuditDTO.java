package cn.lili.modules.stock.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.io.Serializable;

/**
 * 商家出库单审核请求数据传输对象
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Data
@Schema(description = "出库单审核参数")
public class StockOutboundAuditDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @NotNull(message = "审核结果不能为空")
    @Schema(description = "是否通过：true通过，false驳回")
    private Boolean pass;

    @Schema(description = "审核备注/驳回原因")
    private String auditRemark;
}
