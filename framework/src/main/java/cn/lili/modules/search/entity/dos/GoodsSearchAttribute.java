package cn.lili.modules.search.entity.dos;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * 商品属性
 */
@Data
@NoArgsConstructor
public class GoodsSearchAttribute implements Serializable {

    private static final long serialVersionUID = 4018042777559970062L;

    /**
     * 属性参数：0->规格；1->参数
     */
    private Integer type;

    private String nameId;

    private String name;

    private String valueId;

    private String value;

    private Integer sort;

    public GoodsSearchAttribute(Integer type, String nameId, String name, String valueId, String value, Integer sort) {
        this.type = type;
        this.nameId = nameId;
        this.name = name;
        this.valueId = valueId;
        this.value = value;
        this.sort = sort;
    }
}
