package cn.lili.modules.goods.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;

/**
 * 商品计量单位
 *
 * @author Bulbasaur
 * @since 2020/11/26 16:08
 */
@Data
@TableName("li_goods_unit")
@ApiModel(value = "商品计量单位")
public class GoodsUnit extends BaseEntity {

    @NotEmpty(message = "计量单位名称不能为空")
    @Size(max = 5, message = "计量单位长度最大为5")
    @ApiModelProperty(value = "计量单位名称")
    private String name;
}
