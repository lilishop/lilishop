package cn.lili.modules.goods.entity.vos;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 分类规格VO
 *
 * @author pikachu
 * @date 2020-02-26 23:24:13
 */
@Data
public class CategorySpecificationVO {
    /**
     * 规格id
     */
    @ApiModelProperty(value = "规格id", required = true)
    private String id;

    /**
     * 规格名称
     */
    @ApiModelProperty(value = "规格名称", required = true)
    private String name;
}
