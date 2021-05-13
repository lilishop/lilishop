package cn.lili.modules.goods.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 商品关联参数
 *
 * @author pikachu
 * @date 2020-02-23 9:14:33
 */
@Data
@Entity
@Table(name = "li_goods_params")
@TableName("li_goods_params")
@ApiModel(value = "商品关联参数")
public class GoodsParams extends BaseEntity {

    private static final long serialVersionUID = 1L;

    /**
     * 商品id
     */
    @TableField(value = "goods_id")
    @ApiModelProperty(value = "商品id", hidden = true)
    private String goodsId;
    /**
     * 参数id
     */
    @TableField(value = "param_id")
    @ApiModelProperty(value = "参数id", required = true)
    private String paramId;
    /**
     * 参数名字
     */
    @TableField(value = "param_name")
    @ApiModelProperty(value = "参数名字", required = true)
    private String paramName;
    /**
     * 参数值
     */
    @TableField(value = "param_value")
    @ApiModelProperty(value = "参数值", required = true)
    @Length(max = 100, message = "参数值字符不能大于120")
    private String paramValue;

}