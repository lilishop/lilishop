package cn.lili.modules.goods.entity.dos;

import cn.lili.base.IdEntity;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;

/**
 * 商品规格项
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
@Data
@Entity
@Table(name = "li_specification")
@TableName("li_specification")
@ApiModel(value = "规格项")
public class Specification extends IdEntity {

    private static final long serialVersionUID = 147792597901239486L;

    /**
     * 规格名称
     */
    @NotEmpty(message = "规格名称不能为空")
    @ApiModelProperty(value = "规格名称", required = true)
    private String specName;

    /**
     * 所属卖家 0属于平台
     * <p>
     * 店铺自定义规格暂时废弃 2021-06-23 后续推出新配置方式
     */
    @ApiModelProperty(hidden = true)
    private String storeId;

    /**
     * 规格值名字
     */
    @TableField(value = "spec_value")
    @Column(columnDefinition = "TEXT")
    @ApiModelProperty(value = "规格值名字, 《,》分割")
    private String specValue;


}