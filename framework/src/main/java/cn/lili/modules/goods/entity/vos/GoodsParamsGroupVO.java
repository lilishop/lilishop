package cn.lili.modules.goods.entity.vos;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 商品参数vo
 *
 * @author pikachu
 * @since 2020-02-26 23:24:13
 */
@Data
public class GoodsParamsGroupVO implements Serializable {
    private static final long serialVersionUID = 1450550797436233753L;
    @ApiModelProperty("参数组关联的参数集合")
    private List<GoodsParamsVO> params;
    @ApiModelProperty("参数组名称")
    private String groupName;
    @ApiModelProperty("参数组id")
    private String groupId;


}
