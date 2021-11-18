package cn.lili.modules.goods.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.Max;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.math.BigDecimal;

/**
 * 店铺商品分类
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@Data
@TableName("li_store_goods_label")
@ApiModel(value = "店铺商品分类")
public class StoreGoodsLabel extends BaseEntity {

    private static final long serialVersionUID = -5292518678940634419L;

    @ApiModelProperty("店铺ID")
    private String storeId;

    @NotEmpty(message = "店铺商品分类名称不能为空")
    @Length(max = 20,message = "店铺商品分类名称太长")
    @ApiModelProperty("店铺商品分类名称")
    private String labelName;


    @NotNull(message = "店铺商品分类排序不能为空")
    @Max(value = 99999,message = "排序值太大")
    @ApiModelProperty("店铺商品分类排序")
    private BigDecimal sortOrder;

    @NotEmpty(message = "父节点不能为空，需设定根节点或者某节点的子节点")
    @ApiModelProperty(value = "父id, 根节点为0")
    private String parentId;

    @ApiModelProperty(value = "层级, 从0开始")
    private Integer level;


}
