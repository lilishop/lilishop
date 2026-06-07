package cn.lili.modules.promotion.entity.dos;

import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 限时直降活动
 */
@Data
@TableName("li_flash_discount")
@Schema(description = "限时直降")
@EqualsAndHashCode(callSuper = true)
public class FlashDiscount extends BasePromotions {

    private static final long serialVersionUID = 1L;

    @Schema(description = "限购数量，0表示不限")
    private Integer limitNum;

    @Schema(description = "活动说明")
    private String description;
}
