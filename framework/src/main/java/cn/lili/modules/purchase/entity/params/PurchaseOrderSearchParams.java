package cn.lili.modules.purchase.entity.params;

import cn.lili.common.vo.PageVO;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 供求单查询参数
 *
 * @author Bulbasaur
 * @since 2020/11/27 11:29
 */
@Data
public class PurchaseOrderSearchParams extends PageVO {

    @ApiModelProperty(value = "会员ID")
    private String memberId;

    @ApiModelProperty(value = "分类ID")
    private String categoryId;

    @ApiModelProperty(value = "状态，开启：OPEN，关闭：CLOSE")
    private String status;
}
