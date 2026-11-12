package cn.lili.modules.stock.entity.dto;

import cn.lili.modules.stock.entity.enums.StockFlowTypeEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 库存流水上下文参数对象
 * 用于在调用库存变动方法时传递业务来源、单号及操作人信息；为空时不记录流水
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "库存流水上下文")
public class StockFlowContext implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "变动类型")
    private StockFlowTypeEnum flowType;

    @Schema(description = "业务单号/批次号")
    private String bizSn;

    @Schema(description = "操作人")
    private String operator;

    @Schema(description = "备注")
    private String remark;

    /**
     * 静态构造便捷方法
     *
     * @param flowType 变动类型
     * @param bizSn    业务单号/批次号
     * @param operator 操作人
     * @param remark   备注
     * @return 上下文对象
     */
    public static StockFlowContext of(StockFlowTypeEnum flowType, String bizSn, String operator, String remark) {
        return StockFlowContext.builder()
                .flowType(flowType)
                .bizSn(bizSn)
                .operator(operator)
                .remark(remark)
                .build();
    }
}
