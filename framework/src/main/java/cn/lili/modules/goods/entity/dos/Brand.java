package cn.lili.modules.goods.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;

/**
 * 商品品牌
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
@Data
@Entity
@Table(name = "li_brand")
@TableName("li_brand")
@ApiModel(value = "商品品牌")
public class Brand extends BaseEntity {


    private static final long serialVersionUID = -8236865838438521426L;
    /**
     * 品牌名称
     */
    @NotEmpty(message = "品牌名称不能为空")
    @ApiModelProperty(value = "品牌名称", required = true)
    private String name;
    /**
     * 品牌图标
     */
    @NotEmpty(message = "品牌图标不能为空")
    @ApiModelProperty(value = "品牌图标", required = true)
    private String logo;

}