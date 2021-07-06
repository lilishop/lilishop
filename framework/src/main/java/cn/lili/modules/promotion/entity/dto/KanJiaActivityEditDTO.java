package cn.lili.modules.promotion.entity.dto;


import cn.lili.modules.promotion.entity.dos.KanJiaActivityGoods;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.Date;

/**
 * 砍价活动通用类
 *
 * @author qiuqiu
 * @date 2020/8/21
 **/
@Data
public class KanJiaActivityEditDTO implements Serializable {


    private static final long serialVersionUID = -1378599087650538592L;
    @NotEmpty(message = "活动名称不能为空")
    @ApiModelProperty(value = "活动名称", required = true)
    private String promotionName;

    @ApiModelProperty(value = "每人最低砍价金额", required = true)
    @NotEmpty(message = "每人最低砍价金额不能为空")
    private Double lowestPrice;

    @ApiModelProperty(value = "每人最高砍价金额", required = true)
    @NotEmpty(message = "每人最高砍价金额不能为空")
    private Double highestPrice;

    @Min(message = "活动开始时间不能为空", value = 0)
    @ApiModelProperty(value = "活动开始时间", required = true)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date startTime;

    @Min(message = "活动结束时间不能为空", value = 0)
    @ApiModelProperty(value = "活动结束时间", required = true)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date endTime;

    @ApiModelProperty(value = "砍价活动商品列表")
    IPage<KanJiaActivityGoods> promotionGoodsList;

}
