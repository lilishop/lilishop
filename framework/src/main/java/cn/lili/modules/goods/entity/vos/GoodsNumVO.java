package cn.lili.modules.goods.entity.vos;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

@Data
public class GoodsNumVO {

    @ApiModelProperty(value = "出售中的商品数量")
    private Integer upperGoodsNum;
    @ApiModelProperty(value = "仓库中的商品数量")
    private Integer downGoodsNum;
    @ApiModelProperty(value = "待审核商品数量")
    private Integer auditGoodsNum;
    @ApiModelProperty(value = "审核未通过数量")
    private Integer refuseGoodsNum;
}
