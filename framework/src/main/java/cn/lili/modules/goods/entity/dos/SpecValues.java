package cn.lili.modules.goods.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 规格值
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
@Data
@Entity
@Table(name = "li_spec_values")
@TableName("li_spec_values")
@ApiModel(value = "规格值")
public class SpecValues extends BaseEntity {

    private static final long serialVersionUID = 1L;

    /**
     * 规格项id
     */
    @TableField(value = "spec_id")
    @ApiModelProperty(value = "规格项id")
    private String specId;

    /**
     * 规格值名字
     */
    @TableField(value = "spec_value")
    @ApiModelProperty(value = "规格值名字")
    private String specValue;

}