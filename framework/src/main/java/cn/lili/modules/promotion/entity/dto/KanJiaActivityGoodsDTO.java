package cn.lili.modules.promotion.entity.dto;


import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.Date;

/**
 * 砍价活动商品
 *
 * @author qiuqiu
 * @date 2020/8/21
 **/
@Data
public class KanJiaActivityGoodsDTO implements Serializable {


    private static final long serialVersionUID = 1969340823809319805L;

    @ApiModelProperty(value = "结算价格")
    @NotEmpty(message = "结算价格不能为空")
    private Double settlementPrice;

    @ApiModelProperty(value = "最低购买金额")
    @NotEmpty(message = "最低购买金额不能为空")
    private Double purchasePrice;

    @ApiModelProperty(value = "货品id")
    @NotEmpty(message = "货品id不能为空")
    private String skuId;

    @ApiModelProperty(value = "活动库存")
    @NotEmpty(message = "活动库存不能为空")
    private Integer stock;

    @ApiModelProperty(value = "每人最低砍价金额")
    @NotEmpty(message = "每人最低砍价金额不能为空")
    private Double lowestPrice;

    @NotEmpty(message = "每人最高砍价金额不能为空")
    @ApiModelProperty(value = "每人最高砍价金额")
    private Double highestPrice;

    @Min(message = "活动开始时间不能为空", value = 0)
    @ApiModelProperty(value = "活动开始时间", required = true)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date startTime;

    @Min(message = "活动结束时间不能为空", value = 0)
    @ApiModelProperty(value = "活动结束时间", required = true)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date endTime;

}
