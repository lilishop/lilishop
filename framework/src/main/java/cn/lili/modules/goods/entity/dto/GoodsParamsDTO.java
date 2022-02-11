package cn.lili.modules.goods.entity.dto;

import com.baomidou.mybatisplus.annotation.TableField;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 商品关联参数
 *
 * @author pikachu
 * @since 2020-02-23 9:14:33
 */
@Data
@ApiModel(value = "商品参数分组")
public class GoodsParamsDTO implements Serializable {

    private static final long serialVersionUID = 4892783539320159200L;

    @TableField(value = "group_id")
    @ApiModelProperty(value = "分组id")
    private String groupId;

    @TableField(value = "group_name")
    @ApiModelProperty(value = "分组名称")
    private String groupName;

    @ApiModelProperty(value = "分组内的商品参数列表")
    private List<GoodsParamsItemDTO> goodsParamsItemDTOList;

}