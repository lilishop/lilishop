package cn.lili.modules.goods.entity.vos;

import cn.lili.modules.goods.entity.dos.Specification;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 规格VO
 *
 * @author Chopper
 * @date 2020-02-26 23:24:13
 */
@Data
@NoArgsConstructor
public class SpecificationVO extends Specification {

    private static final long serialVersionUID = 5504602856844228350L;

    @ApiModelProperty(value = "规格项名称")
    private String specValue;

    @ApiModelProperty(value = "分类path")
    private String categoryPath;

    public SpecificationVO(String specName, String storeId, String categoryPath) {

        this.setSpecName(specName);
        this.setStoreId(storeId);
        this.categoryPath = categoryPath;


    }

}
