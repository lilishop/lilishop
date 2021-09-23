package cn.lili.modules.goods.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;

/**
 * 商品品牌
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@Data
@TableName("li_brand")
@ApiModel(value = "商品品牌")
public class Brand extends BaseEntity {


    private static final long serialVersionUID = -8236865838438521426L;

    @NotEmpty(message = "品牌名称不能为空")
    @Length(max = 20, message = "品牌名称应该小于20长度字符")
    @ApiModelProperty(value = "品牌名称", required = true)
    private String name;

    @NotEmpty(message = "品牌图标不能为空")
    @Length(max = 255, message = "品牌图标地址长度超过255字符")
    @ApiModelProperty(value = "品牌图标", required = true)
    private String logo;

}