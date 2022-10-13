package cn.lili.modules.goods.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author paulG
 * @since 2022/5/20
 **/
@Data
@EqualsAndHashCode(callSuper = true)
@TableName("li_wholesale")
@ApiModel(value = "批发商品")
public class Wholesale extends BaseIdEntity {

    private static final long serialVersionUID = -6389806138583086068L;

    @ApiModelProperty(value = "商品ID")
    private String goodsId;
    @ApiModelProperty(value = "SkuID")
    private String skuId;
    @ApiModelProperty(value = "模版id")
    private String templateId;
    @ApiModelProperty(value = "数量")
    private Integer num;
    @ApiModelProperty(value = "金额")
    private Double price;
}
