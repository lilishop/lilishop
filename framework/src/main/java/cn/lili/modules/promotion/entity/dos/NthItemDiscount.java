package cn.lili.modules.promotion.entity.dos;

import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 第N件优惠活动
 */
@Data
@TableName("li_nth_item_discount")
@Schema(description = "第N件优惠")
@EqualsAndHashCode(callSuper = true)
public class NthItemDiscount extends BasePromotions {

    private static final long serialVersionUID = 1L;

    @Schema(description = "第N件")
    private Integer nthNum;

    @Schema(description = "优惠方式 HALF/FREE/RATE")
    private String discountType;

    @Schema(description = "折扣值")
    private Double discountValue;

    @Schema(description = "活动说明")
    private String description;
}
