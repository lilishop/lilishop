package cn.lili.modules.promotion.entity.dto;


import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.Min;
import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * 砍价活动商品操作DTO
 *
 * @author qiuqiu
 * @date 2020/8/21
 **/
@Data
public class KanjiaActivityGoodsOperationDTO implements Serializable {


    private static final long serialVersionUID = -1378599087650538592L;

    @Min(message = "活动开始时间不能为空", value = 0)
    @ApiModelProperty(value = "活动开始时间", required = true)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date startTime;

    @Min(message = "活动结束时间不能为空", value = 0)
    @ApiModelProperty(value = "活动结束时间", required = true)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date endTime;

    @ApiModelProperty(value = "砍价活动商品列表")
    List<KanjiaActivityGoodsDTO> promotionGoodsList;

}
