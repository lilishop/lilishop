package cn.lili.modules.search.entity.dos;

import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import java.io.Serializable;

/**
 * 商品属性索引
 *
 * @author paulG
 * @since 2020/10/14
 **/
@Data
@NoArgsConstructor
public class EsGoodsAttribute implements Serializable {

    private static final long serialVersionUID = 4018042777559970062L;

    /**
     * 属性参数：0->规格；1->参数
     */
    @Field(type = FieldType.Integer)
    private Integer type;

    /**
     * 属性名称
     */
    private String nameId;

    /**
     * 属性名称
     */
    @Field(type = FieldType.Text, fielddata = true)
    private String name;

    /**
     * 属性值
     */
    @Field(type = FieldType.Text)
    private String valueId;

    /**
     * 属性值
     */
    @Field(type = FieldType.Text, fielddata = true)
    private String value;


    /**
     * 排序
     */
    @Field(type = FieldType.Integer)
    private Integer sort;

    public EsGoodsAttribute(Integer type, String nameId, String name, String valueId, String value, Integer sort) {
        this.type = type;
        this.nameId = nameId;
        this.name = name;
        this.valueId = valueId;
        this.value = value;
        this.sort = sort;
    }
}
