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
 * 商品计量单位
 *
 * @author Bulbasaur
 * @date: 2020/11/26 16:08
 */
@Data
@Entity
@Table(name = "li_goods_unit")
@TableName("li_goods_unit")
@ApiModel(value = "商品计量单位")
public class GoodsUnit extends BaseEntity {

    @NotEmpty(message = "计量单位名称不能为空")
    @ApiModelProperty(value = "计量单位名称")
    private String name;
}
