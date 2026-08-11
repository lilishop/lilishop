package cn.lili.modules.goods.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 分类品牌VO
 *
 * @author pikachu
 * @since 2020-02-26 23:24:13
 */
@Data
public class CategoryBrandVO {
    /**
     * 品牌id
     */
    @Schema(description = "品牌id", requiredMode = Schema.RequiredMode.REQUIRED)
    private String id;

    /**
     * 品牌名称
     */
    @Schema(description = "品牌名称", requiredMode = Schema.RequiredMode.REQUIRED)
    private String name;
}
