package cn.lili.modules.procurement.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = false)
@TableName("li_damage_report_item")
/**
 * 报损单明细实体，记录商品的报损数量与金额
 * @author Bulbasaur
 * @since 2025-12-18
 */
public class DamageReportItem extends BaseEntity {

    @Schema(description = "报损单ID")
    private String reportId;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "报损数量")
    private Integer quantity;

    @Schema(description = "单价")
    private Double unitPrice;

    @Schema(description = "金额")
    private Double amount;
}
