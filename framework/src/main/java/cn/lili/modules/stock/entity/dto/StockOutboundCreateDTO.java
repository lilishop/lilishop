package cn.lili.modules.stock.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 商家出库单创建与编辑数据传输对象
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Data
@Schema(description = "创建出库单参数")
public class StockOutboundCreateDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @NotBlank(message = "出库原因不能为空")
    @Schema(description = "出库原因ID")
    private String reasonId;

    @Schema(description = "出库原因名称")
    private String reasonName;

    @Schema(description = "出库备注")
    private String remark;

    @Valid
    @NotEmpty(message = "出库明细不能为空")
    @Schema(description = "出库明细列表")
    private List<StockOutboundItemDTO> items;
}
