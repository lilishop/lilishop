package cn.lili.modules.store.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;
import java.math.BigDecimal;

/**
 * 店铺商品分类
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
@Data
@Entity
@Table(name = "li_store_goods_label")
@TableName("li_store_goods_label")
@ApiModel(value = "店铺商品分类")
public class StoreGoodsLabel extends BaseEntity {

    private static final long serialVersionUID = -5292518678940634419L;

    @ApiModelProperty("店铺ID")
    private String storeId;

    @ApiModelProperty("店铺商品分类名称")
    private String labelName;

    @ApiModelProperty("店铺商品分类排序")
    private BigDecimal sortOrder;

    @ApiModelProperty(value = "父id, 根节点为0")
    private String parentId;

    @ApiModelProperty(value = "层级, 从0开始")
    private Integer level;


}
