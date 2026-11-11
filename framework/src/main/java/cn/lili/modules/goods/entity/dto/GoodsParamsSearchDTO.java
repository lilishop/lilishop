package cn.lili.modules.goods.entity.dto;

import cn.lili.common.vo.PageVO;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 商品参数查询参数
 *
 * @author Bulbasaur
 * @since 2020-02-23 9:14:33
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class GoodsParamsSearchDTO extends PageVO {

    @Schema(description = "参数名称", requiredMode = Schema.RequiredMode.REQUIRED)
    private String paramName;
}
