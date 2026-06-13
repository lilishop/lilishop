package cn.lili.modules.goods.entity.dto;

import cn.lili.common.vo.PageVO;
import tools.jackson.databind.PropertyNamingStrategies;
import tools.jackson.databind.annotation.JsonNaming;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 商品品牌dto
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@Data
@EqualsAndHashCode(callSuper = false)
@JsonNaming(value = PropertyNamingStrategies.SnakeCaseStrategy.class)
@Schema(description = "商品品牌dto")
public class BrandPageDTO extends PageVO {

    private static final long serialVersionUID = 8906820486037326039L;

    @Schema(description = "品牌名称")
    private String name;
}
