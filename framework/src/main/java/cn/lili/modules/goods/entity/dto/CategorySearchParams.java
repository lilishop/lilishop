package cn.lili.modules.goods.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 分类查询参数
 *
 * @author paulG
 * @since 2020/12/1
 **/
@Data
public class CategorySearchParams {

    @ApiModelProperty(value = "分类名称")
    private String name;

    @ApiModelProperty(value = "父id")
    private String parentId;

    @ApiModelProperty(value = "层级")
    private Integer level;

    @ApiModelProperty(value = "排序值")
    private BigDecimal sortOrder;

    @ApiModelProperty(value = "佣金比例")
    private BigDecimal commissionRate;

    @ApiModelProperty(value = "父节点名称")
    private String parentTitle;

}
